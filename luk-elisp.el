;;; -*- coding: utf-8; lexical-binding: t -*-

(provide 'luk-elisp)

(defun luk-elisp-setup ()
  (when (require 'company nil 'noerror)
    (add-hook 'emacs-lisp-mode-hook 'company-mode))
  (define-key emacs-lisp-mode-map (kbd "C-c g") 'xref-find-definitions))
