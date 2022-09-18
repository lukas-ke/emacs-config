;;; -*- coding: utf-8; lexical-binding: t -*-

(provide 'luk-whitespace)

(defvar-local luk-should-trim-whitespace t
  "Controls whether ‘luk-maybe-trim-whitespace’ should trim.

The main use of this variable is to allow excluding whitespace
trimming from specific modes when ‘luk-maybe-trim-whitespace’ has
been added to the ‘before-save-hook’. The variable should in that case
be set to nil in the mode-hook for that mode.")

(defun luk-squash-consecutive-blank-lines ()
  "Turn consecutive blank lines into a single blank line."
  (save-match-data
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward
              "^\^j\^j+"
              nil ; unbounded
              t); no error
        (replace-match "\^j")))))

(defun luk-maybe-trim-whitespace ()
  "Trim trailing (and some other) whitespace in buffer."
  (when luk-should-trim-whitespace
    (delete-trailing-whitespace))
  (if (string-suffix-p ".ts" (buffer-file-name))
      ;; tslint doesn't allow multiple blank lines
      (luk-squash-consecutive-blank-lines)))
