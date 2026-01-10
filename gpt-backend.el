;;; gpt-backend.el --- Backend definitions for gpt.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; Keywords: openai, anthropic, claude, language, copilot, convenience, tools
;; URL: https://github.com/stuhlmueller/gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file defines the backend system for gpt.el.
;; Backends represent different LLM providers (OpenAI, Anthropic, Google).
;; Each backend implements generic methods for request building and response parsing.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'json)

;;; Backend base class

(defclass gpt-backend ()
  ((name
    :initarg :name
    :type string
    :documentation "Display name for the backend.")
   (url
    :initarg :url
    :type string
    :documentation "API endpoint URL.")
   (key
    :initarg :key
    :type (or string null)
    :documentation "API key for authentication.")
   (models
    :initarg :models
    :type list
    :documentation "List of available model IDs.")
   (default-model
    :initarg :default-model
    :type string
    :documentation "Default model ID to use."))
  :documentation "Base class for LLM backends.")

;;; Generic methods

(cl-defgeneric gpt-backend-headers (backend)
  "Return HTTP headers for requests to BACKEND.")

(cl-defgeneric gpt-backend-request-data (backend messages options)
  "Build request data for BACKEND from MESSAGES and OPTIONS.
MESSAGES is a list of message plists with :role and :content keys.
OPTIONS is a plist with :model, :max-tokens, :temperature, etc.")

(cl-defgeneric gpt-backend-parse-response (backend response)
  "Parse the JSON RESPONSE from BACKEND and return the content string.
May also return additional metadata as a plist.")

(cl-defgeneric gpt-backend-parse-stream-chunk (backend chunk state)
  "Parse a streaming CHUNK from BACKEND using STATE for context.
Returns a plist with:
  :content - the text content (or nil)
  :thinking - thinking block content (or nil)
  :done - t if this is the final chunk
  :state - updated state for next chunk")

(cl-defgeneric gpt-backend-stream-request-data (backend messages options)
  "Build streaming request data for BACKEND from MESSAGES and OPTIONS.
Returns the request data with stream=true added.")

;;; Helper functions

(defun gpt-backend--json-encode (data)
  "Encode DATA as JSON string."
  (let ((json-false :json-false)
        (json-null :json-null))
    (json-encode data)))

(defun gpt-backend--json-read ()
  "Read JSON from current buffer."
  (let ((json-object-type 'plist)
        (json-array-type 'vector)
        (json-key-type 'keyword)
        (json-false nil)
        (json-null nil))
    (json-read)))

(defun gpt-backend--json-read-from-string (str)
  "Parse JSON from STR."
  (let ((json-object-type 'plist)
        (json-array-type 'vector)
        (json-key-type 'keyword)
        (json-false nil)
        (json-null nil))
    (json-read-from-string str)))

(defun gpt-backend--parse-messages (text)
  "Parse TEXT into a list of message plists.
Recognizes User:/Assistant: and Human:/Model: prefixes."
  (let ((messages nil)
        (patterns '(("User:\\s-*" . "user")
                    ("Human:\\s-*" . "user")
                    ("Assistant:\\s-*" . "assistant")
                    ("Model:\\s-*" . "assistant"))))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (let ((current-role nil)
            (current-content nil)
            (content-start nil))
        ;; Find all role markers
        (while (not (eobp))
          (let ((found-role nil))
            ;; Check for role prefix at line start
            (when (bolp)
              (dolist (pattern patterns)
                (when (looking-at (car pattern))
                  (setq found-role (cdr pattern))
                  (goto-char (match-end 0)))))
            (if found-role
                (progn
                  ;; Save previous message if we have one
                  (when (and current-role content-start)
                    (let ((content (string-trim
                                    (buffer-substring-no-properties
                                     content-start (line-beginning-position)))))
                      (unless (string-empty-p content)
                        (push (list :role current-role :content content)
                              messages))))
                  ;; Start new message
                  (setq current-role found-role)
                  (setq content-start (point)))
              (forward-line 1))))
        ;; Add final message
        (when (and current-role content-start)
          (let ((content (string-trim
                          (buffer-substring-no-properties
                           content-start (point-max)))))
            (unless (string-empty-p content)
              (push (list :role current-role :content content) messages))))))
    (nreverse messages)))

(provide 'gpt-backend)
;;; gpt-backend.el ends here
