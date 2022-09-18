;;; -*- coding: utf-8; lexical-binding: t -*-

(provide 'luk-util)

(defun -up (&optional path n)
  "Get the N-th parent folder of PATH or `buffer-file-name'."
  (or path (setq path (buffer-file-name)))

  (if n
    (let ((folder path))
      (dotimes (num n)
        (setq path (file-name-directory (directory-file-name path))))
      path)

    (file-name-directory (directory-file-name path))))

(defun luk-in-comment ()
  "Return t if point is inside a code comment."
  (if (nth 4 (syntax-ppss)) t nil))
