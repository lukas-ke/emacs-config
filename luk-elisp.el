;;; -*- coding: utf-8; lexical-binding: t -*-

(provide 'luk-elisp)
(require 'luk-util)
(require 'luk-tab-complete)
(require 'luk-hydra)

(defun luk-elisp-insert-boilerplate ()
  "Insert magic comment for utf-8-coding and lexical binding"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert ";;; -*- coding: utf-8; lexical-binding: t -*-\n\n")))


;; Context hydra
(defun luk-elisp-show-context-hydra ()
  (interactive)
  (let ((sym (symbol-at-point)))
    (if (functionp sym)
        (luk-elisp-function-hydra/body)
      (user-error (format "Context hydra: Not implemented for the %s's type" sym)))))

(defun luk-elisp--hook()
  "Run luk-elisp-insert-boilerplate for new Emacs-lisp files."

  (setq luk-context-hydra #'luk-elisp-show-context-hydra)

  (when (require 'company nil 'noerror)
    (company-mode))

  (if (require 'form-feed nil 'noerror)
      (form-feed-mode)
    (push '("" . "▬" ) prettify-symbols-alist)
    (prettify-symbols-mode))

  (when (luk-new-file-buffer-p)
    (luk-elisp-insert-boilerplate)
    (forward-line)
    (forward-line)))



(defhydra luk-elisp-function-hydra (:hint nil :foreign-keys warn)
  (format "\
Main ➤ %s %%-30(symbol-at-point)^^^^^^^^^^^^^^ _._: up
^─^───────────────────────────────────────────────────
_g_: find definitions _G_: other window
_d_: describe function"
          (luk-caption "function"))
  ("." (luk-hydra-push 'luk-org-hydra/body "org") :exit t)
  ("g" xref-find-definitions :exit t)
  ("G" xref-find-definitions-other-window :exit t)
  ("d" (describe-function (symbol-at-point)) :exit t)
  ("q" nil :exit t))


;; Setup function
(defun luk-elisp-setup ()
  (add-hook 'emacs-lisp-mode-hook 'luk-elisp--hook)
  (define-key emacs-lisp-mode-map (kbd "C-c g") 'xref-find-definitions-other-window)
  (define-key emacs-lisp-mode-map (kbd "<tab>") 'luk-tab-complete-smart-tab))
