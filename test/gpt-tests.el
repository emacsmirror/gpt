;;; gpt-tests.el --- ERT tests for gpt.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Andreas Stuhlmueller

;; Author: Test Suite
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Basic regression tests for helper functions in gpt.el.

;;; Code:

(require 'ert)
(require 'gpt-ui)
(require 'gpt-core)

(ert-deftest gpt-clean-edit-output-strips-thinking-and-prefix ()
  "Ensure thinking markers and assistant prefixes are removed."
  (let ((input "[Thinking...]\nPlan\n[Thinking done.]\nAssistant: Final text"))
    (should (equal (gpt--clean-edit-output input) "Final text"))))

(ert-deftest gpt-get-context-current-buffer-with-region ()
  "Context should include cursor marker and selected region."
  (with-temp-buffer
    (insert "first line\nsecond line")
    (goto-char (point-min))
    (push-mark (point) t t)
    (forward-word 2)
    (let* ((transient-mark-mode t)
           (mark-active t)
           (context (gpt-get-context 'current-buffer)))
      (should (string-match "<cursor/>" context))
      (should (string-match "Selected region:" context))
      (should (string-match "first line" context)))))

(ert-deftest gpt-format-command-with-model-prefixes-command ()
  "Multi-model helper should prefix the command with the model id."
  (should (equal (gpt--format-command-with-model "Do things" "gpt-5")
                 "[gpt-5] Do things")))

(require 'gpt-api)
(require 'gpt-mode)

(ert-deftest gpt-message-formats-correctly ()
  "Verify gpt-message prepends GPT prefix."
  (should (equal "GPT: Hello world"
                 (let ((messages nil))
                   (cl-letf (((symbol-function 'message)
                              (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
                     (gpt-message "Hello %s" "world"))
                   (car messages)))))

(ert-deftest gpt-buffer-string-returns-content ()
  "Verify gpt-buffer-string returns buffer text."
  (with-temp-buffer
    (insert "Buffer content here")
    (should (equal "Buffer content here" (gpt-buffer-string (current-buffer))))))

(provide 'gpt-tests)

;;; gpt-tests.el ends here
