;;; -*- coding: utf-8; lexical-binding: t -*-
;; Function `luk-insert-closing-delimiter' for inserting the closing
;; delimiter for any open delimiter (like ({[")

(defun luk--opposite-delimiter (delimiter)
  "Return the opposite delimiter for DELIMITER.

DELIMITER should be some brace type."
  (cl-assert (stringp delimiter))
  (cond ((string-equal delimiter "{") "}")
        ((string-equal delimiter "(") ")")
        ((string-equal delimiter "[") "]")
        (t (error "Unknown delimiter %s" delimiter))))

(defun luk--opposite-string-delimiter (delimiter)
  "Return the closing delimiter for string-delimiter DELIMITER.

If DELIMITER is t, this will be the generic string delimiter for
the major mode, if known."
  (cond
   ((characterp delimiter) (make-string 1 delimiter))
   ((and (booleanp delimiter) delimiter)
    (cond
     ((eq major-mode 'lua-mode) "]]")
     ((eq major-mode 'python-mode) "\"\"\"")
     (t (error "Generic string delimiter for %s unknown" major-mode))))
   (t (error "Unexpected delimiter type for %s" delimiter))))

(defun luk-insert-closing-delimiter ()
  "Insert the closing delimiter for the last open delimiter at point.

The delimiter may match {, (, [, \" and \\=' depending on which
syntax table is active (see `syntax-ppss').

For example, in Python, when at the end of \"([{\", this would
insert \"}\", and for successive invocations \"]\" then \")\"."
  (interactive)
  (let* ((ppss (syntax-ppss))
         (open-paren-indexes (nth 9 ppss))
         (in-string (nth 3 ppss)))
    (if in-string
        (insert (luk--opposite-string-delimiter in-string))
      (if open-paren-indexes
          (let ((delim (buffer-substring (car (last open-paren-indexes)) (+ 1 (car (last open-paren-indexes))))))
            ;; TODO: This doesn't say e.g. Matches: like when inserting a parenthesis normally.
            (insert (luk--opposite-delimiter delim)))
        (message "Everything closed.")))))

(provide 'luk-insert-closing-delimiter)
