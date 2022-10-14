;;; -*- coding: utf-8; lexical-binding: t -*-

(defvar luk-diminished nil)

(defun luk-toggle-diminish ()
  (interactive)
  (if luk-diminished (luk-diminish-undo) (luk-diminish)))

(if (require 'diminish nil 'noerror)
    (progn
      (defun luk-diminish ()
        ;; Use diminish package to remove some minor-modes from modeline.
        (with-eval-after-load "perfect-margin" (diminish 'perfect-margin-mode))
        (with-eval-after-load "yasnippet" (diminish 'yas-minor-mode))
        (with-eval-after-load "isearch" (diminish 'isearch-mode))
        (with-eval-after-load "company" (diminish 'company-mode))
        (with-eval-after-load "eldoc" (diminish 'eldoc-mode))
        (with-eval-after-load "lsp-lens" (diminish 'lsp-lens-mode))
        (with-eval-after-load "flycheck" (diminish 'flycheck-mode))
        (with-eval-after-load "lsp-mode" (diminish 'lsp-mode "lsp"))
        (with-eval-after-load "autorevert" (diminish 'auto-revert-mode))
        (with-eval-after-load "form-feed" (diminish 'form-feed-mode))
        (with-eval-after-load "org-indent" (diminish 'org-indent-mode))
        (with-eval-after-load "org-capture" (diminish 'org-capture-mode))
        (with-eval-after-load "company" (diminish 'company-mode))
        (setq luk-diminished t))

      (defun luk-diminish-undo ()
        (diminish-undo 'diminished-modes)
        (setq luk-diminished nil)))

  ;; Handle package missing
  (defun luk-diminish ()
    (message "Error: diminish package not installed")
    nil)
  (defun luk-diminish-undo ()
    (message "Error: diminish package not installed")
    nil))

(provide 'luk-diminish)
