;;; -*- coding: utf-8; lexical-binding: t -*-

;; Utilities for reading from minibuffer

(provide 'luk-minibuffer)

(defun luk-completing-read-multiple
    (PROMPT
     TABLE
     &optional PREDICATE
     REQUIRE-MATCH
     INITIAL-INPUT
     HIST
     DEF
     INHERIT-INPUT-METHOD)
  "Specialization of `completing-read-multiple' that uses space as separator."
  (require 'crm)
  (let (
        ;; Use space as the separator instead of the default comma
        (crm-separator " ")
        ;; Local copy of normal completion map
        (crm-local-completion-map (copy-keymap crm-local-completion-map)))
    ;; Insert space when pressed, instead of triggering completion
    (define-key crm-local-completion-map " " 'self-insert-command)
    (completing-read-multiple PROMPT TABLE PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)))

;; Test function
(defun luk-test-completing-read-multiple ()
  (interactive)
  (luk-completing-read-multiple "Prompt text: " '("Hello" "World")))

;; Test function
(defun luk-test-completing-read-multiple-f ()
  (interactive)
  (luk-completing-read-multiple
   "Prompt text: "
   (lambda (STRING PREDICATE whatev)
     ;; Nil to say no-match.
     ;; These completions replace strings
     (message "waa %s" (buffer-string)) ;; content of minibuffer (including prompt :(..)
     (if (string-equal STRING "flarf")
         "florfo"
       "flarf"))))
