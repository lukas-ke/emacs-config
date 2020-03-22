;;; -*- coding: utf-8; lexical-binding: t -*-

(provide 'luk-whitespace)

(defun luk-squash-consecutive-blank-lines ()
  "Turn consecutive blank lines into a single blank line"
  (save-excursion
    (while (re-search-forward
            "^\^j\^j+"
            nil ; unbounded
            t); no error
      (replace-match "\^j"))))

(defun luk-maybe-trim-whitespace ()
  "Trim trailing (and some other) whitespace depending on mode"
  (if (not (string= major-mode "markdown-mode"))
      (delete-trailing-whitespace))
  (if (string-suffix-p ".ts" (buffer-file-name))
      ;; tslint doesn't allow multiple blank lines
      (luk-squash-consecutive-blank-lines)))
