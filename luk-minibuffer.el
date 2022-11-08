;;; -*- coding: utf-8; lexical-binding: t -*-

;; Utilities for reading from minibuffer

(provide 'luk-minibuffer)

(defmacro luk/without-fido (&rest body)
  "Disable fido-mode for the duration of BODY."
  (if (not fido-mode)
      `(progn ,@body)
    `(unwind-protect
         (progn
           (fido-mode -1)
           ,@body)
       (fido-mode))))

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
    ;; Run without fido-mode, otherwise the history doesn't work
    (luk/without-fido
     (completing-read-multiple
      PROMPT
      TABLE
      PREDICATE
      REQUIRE-MATCH
      INITIAL-INPUT
      HIST
      DEF
      INHERIT-INPUT-METHOD))))
