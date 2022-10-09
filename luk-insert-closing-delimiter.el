;;; -*- coding: utf-8; lexical-binding: t -*-
;; Function `luk-insert-closing-delimiter' for inserting the closing
;; delimiter for any open delimiter (like ({[")

(defun luk--opposite-delimiter (delimiter)
  (cond ((string-equal delimiter "{") "}")
        ((string-equal delimiter "(") ")")
        ((string-equal delimiter "[") "]")
        (t "")))

(defun luk-insert-closing-delimiter ()
  "Insert the closing delimiter for the last open delimiter at point.

The delimiter may match {, (, [, \" and \\=' depending on which
syntax table is active (see `syntax-ppss').

For example, in Python, when at the end of \"([{\", this would
insert \"}\", and for successive invocations \"]\" then \")\"."
  (interactive)
  (let* ((ppss (syntax-ppss))
         (open-paren-indexes (nth 9 ppss))
         (quote-char (nth 3 ppss)))
    (if quote-char
        (insert (make-string 1 quote-char))
      (if open-paren-indexes
          (let ((delim (buffer-substring (car (last open-paren-indexes)) (+ 1 (car (last open-paren-indexes))))))
            ;; TODO: This doesn't say e.g. Matches: like when inserting a parenthesis normally.
            (insert (luk--opposite-delimiter delim)))
        (message "Everything closed.")))))

(provide 'luk-insert-closing-delimiter)
