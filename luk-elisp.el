;;; -*- coding: utf-8; lexical-binding: t -*-

(provide 'luk-elisp)

(defun luk-elisp-insert-boilerplate ()
  "Insert magic comment for utf-8-coding and lexical binding"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (insert ";;; -*- coding: utf-8; lexical-binding: t -*-\n\n")))

(defun luk-elisp-maybe-insert-boilerplate ()
  "Run luk-elisp-insert-boilerplate for new Emacs-lisp files."
  (when (and (buffer-file-name) (not (file-exists-p (buffer-file-name))) (= (point) 1))
    (luk-elisp-insert-boilerplate)
    (forward-line)
    (forward-line)))

(defun luk-elisp-setup ()
  (when (require 'company nil 'noerror)
    (add-hook 'emacs-lisp-mode-hook 'company-mode))

  (add-hook 'emacs-lisp-mode-hook 'luk-elisp-maybe-insert-boilerplate)

  (define-key emacs-lisp-mode-map (kbd "C-c g") 'xref-find-definitions))
