;;; gpt-http.el --- HTTP request layer for gpt.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; Keywords: openai, anthropic, claude, language, copilot, convenience, tools
;; URL: https://github.com/stuhlmueller/gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file provides HTTP request functionality for gpt.el.
;; It supports both synchronous requests via url.el and
;; streaming requests via curl for real-time output.

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'url-http)
(require 'gpt-backend)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

;;; Configuration

(defcustom gpt-use-curl t
  "Use curl for API requests when available.
Curl is required for streaming responses."
  :type 'boolean
  :group 'gpt)

(defcustom gpt-curl-program "curl"
  "Path to the curl executable."
  :type 'string
  :group 'gpt)

;;; Error handling

(define-error 'gpt-http-error "GPT HTTP error")
(define-error 'gpt-api-error "GPT API error" 'gpt-http-error)
(define-error 'gpt-auth-error "GPT authentication error" 'gpt-api-error)
(define-error 'gpt-rate-limit-error "GPT rate limit error" 'gpt-api-error)

;;; URL-based requests (non-streaming)

(defun gpt-http--url-request (url headers data callback)
  "Make async HTTP POST request to URL with HEADERS and DATA.
CALLBACK is called with (RESPONSE HTTP-STATUS ERROR)."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         (cons '("Content-Type" . "application/json") headers))
        (url-request-data (encode-coding-string
                           (gpt-backend--json-encode data) 'utf-8)))
    (url-retrieve
     url
     (lambda (status)
       (let (response http-status error-msg)
         (if-let* ((err (plist-get status :error)))
             (setq error-msg (format "Network error: %s" err)
                   http-status 0)
           ;; Parse response
           (set-buffer-multibyte t)
           (goto-char (point-min))
           (when (re-search-forward "HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
             (setq http-status (string-to-number (match-string 1))))
           (when (and url-http-end-of-headers
                      (< url-http-end-of-headers (point-max)))
             (goto-char url-http-end-of-headers)
             (condition-case parse-err
                 (setq response (gpt-backend--json-read))
               (error
                (setq error-msg
                      (format "JSON parse error: %s. Body: %s"
                              (error-message-string parse-err)
                              (buffer-substring-no-properties
                               url-http-end-of-headers
                               (min (+ url-http-end-of-headers 500)
                                    (point-max)))))))))
         ;; Check for API errors
         (when (and response (plist-get response :error))
           (let ((err (plist-get response :error)))
             (setq error-msg
                   (or (plist-get err :message)
                       (format "API error: %s" err)))))
         (kill-buffer (current-buffer))
         (funcall callback response http-status error-msg)))
     nil t nil)))

;;; Curl-based requests (streaming)

(defvar-local gpt-http--curl-process nil
  "The curl process for the current buffer.")

(defvar-local gpt-http--stream-buffer ""
  "Buffer for incomplete streaming data.")

(defvar-local gpt-http--stream-state nil
  "State for stream parsing.")

(defun gpt-http--curl-available-p ()
  "Return non-nil if curl is available."
  (and gpt-use-curl
       (executable-find gpt-curl-program)))

(defun gpt-http--curl-args (url headers)
  "Build curl arguments for URL with HEADERS."
  (append
   (list "--silent"
         "--show-error"
         "--no-buffer"
         "-X" "POST"
         "-H" "Content-Type: application/json")
   ;; Add custom headers
   (mapcan (lambda (h)
             (list "-H" (format "%s: %s" (car h) (cdr h))))
           headers)
   (list url)))

(defun gpt-http-stream-request (url headers data backend on-chunk on-complete)
  "Make streaming HTTP POST request using curl.
URL is the API endpoint.
HEADERS is an alist of HTTP headers.
DATA is the request body (plist).
BACKEND is the gpt-backend instance for parsing.
ON-CHUNK is called with (CONTENT THINKING) for each chunk.
ON-COMPLETE is called with (SUCCESS ERROR-MSG) when done."
  (let* ((json-data (encode-coding-string
                     (gpt-backend--json-encode data) 'utf-8))
         (args (gpt-http--curl-args url headers))
         (process-buffer (generate-new-buffer " *gpt-curl*"))
         (stream-buffer "")
         (stream-state nil)
         proc)
    (with-current-buffer process-buffer
      (setq-local gpt-http--stream-buffer "")
      (setq-local gpt-http--stream-state nil))
    (setq proc
          (make-process
           :name "gpt-curl"
           :buffer process-buffer
           :command (append (list gpt-curl-program) args (list "-d" json-data))
           :coding 'utf-8
           :filter
           (lambda (proc output)
             (when (buffer-live-p (process-buffer proc))
               (with-current-buffer (process-buffer proc)
                 (setq gpt-http--stream-buffer
                       (concat gpt-http--stream-buffer output))
                 ;; Process complete lines
                 (gpt-http--process-stream-data
                  backend
                  (lambda (content thinking)
                    (funcall on-chunk content thinking))))))
           :sentinel
           (lambda (proc event)
             (let ((success (string-match-p "finished" event))
                   (error-msg nil))
               (when (buffer-live-p (process-buffer proc))
                 (with-current-buffer (process-buffer proc)
                   ;; Check for errors in remaining buffer
                   (unless success
                     (setq error-msg
                           (format "Curl process %s: %s"
                                   (string-trim event)
                                   (buffer-string))))
                   ;; Check for API error in response
                   (when (and success
                              (string-match-p "\"error\"" (buffer-string)))
                     (condition-case nil
                         (let* ((json-start (string-match "{" (buffer-string)))
                                (response (when json-start
                                            (gpt-backend--json-read-from-string
                                             (substring (buffer-string) json-start)))))
                           (when-let* ((err (plist-get response :error)))
                             (setq success nil
                                   error-msg (plist-get err :message))))
                       (error nil)))))
               (funcall on-complete success error-msg)
               (when (buffer-live-p (process-buffer proc))
                 (kill-buffer (process-buffer proc)))))))
    proc))

(defun gpt-http--process-stream-data (backend on-data)
  "Process streaming data in current buffer using BACKEND.
ON-DATA is called with (CONTENT THINKING) for each parsed chunk."
  (let ((data gpt-http--stream-buffer)
        (pos 0))
    ;; Process complete lines
    (while (string-match "\n" data pos)
      (let ((line (substring data pos (match-beginning 0))))
        (setq pos (match-end 0))
        ;; Handle SSE data: lines
        (when (string-prefix-p "data: " line)
          (let ((json-str (substring line 6)))
            (unless (string= json-str "[DONE]")
              (condition-case nil
                  (let* ((result (gpt-backend-parse-stream-chunk
                                  backend json-str gpt-http--stream-state))
                         (content (plist-get result :content))
                         (thinking (plist-get result :thinking)))
                    (setq gpt-http--stream-state (plist-get result :state))
                    (when (or content thinking)
                      (funcall on-data content thinking)))
                (error nil)))))))
    ;; Keep unprocessed data
    (setq gpt-http--stream-buffer (substring data pos))))

(defun gpt-http-abort (buffer)
  "Abort any running HTTP request in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and gpt-http--curl-process
                 (process-live-p gpt-http--curl-process))
        (delete-process gpt-http--curl-process)))))

(provide 'gpt-http)
;;; gpt-http.el ends here
