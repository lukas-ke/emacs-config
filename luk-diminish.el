;;; -*- coding: utf-8; lexical-binding: t -*-

(defgroup luk-diminish nil "Diminishing options")

(defcustom luk-should-diminish nil
  "When t, minor modes should be diminished."
  :type 'boolean
  :group 'luk-diminish)

(when (require 'luk nil 'noerror)
  (luk-add-group 'luk-diminish))

(defun luk-toggle-diminish ()
  (interactive)
  (setq luk-should-diminish (not luk-should-diminish))
  (luk-maybe-diminish))

(defun luk-maybe-diminish ()
  "Diminish minor modes if `luk-should-diminish' is t."
  (if luk-should-diminish (luk-diminish) (luk-diminish-undo)))

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
        (with-eval-after-load "company" (diminish 'company-mode)))

      (defun luk-diminish-undo ()
        (diminish-undo 'diminished-modes)))

  ;; Handle package missing
  (defun luk-diminish ()
    (message "Error: diminish package not installed")
    nil)
  (defun luk-diminish-undo ()
    (message "Error: diminish package not installed")
    nil))

(provide 'luk-diminish)
