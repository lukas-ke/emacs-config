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

(defun luk-elisp--hook()
  "Run luk-elisp-insert-boilerplate for new Emacs-lisp files."

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

;; Context hydra
(defun luk-elisp-context-hydra ()
  (interactive)
  (let ((sym (symbol-at-point)))
    (cond ((functionp sym) (luk-elisp-function-hydra/body))
           (t (luk-hydra/body)))))


;; Setup function
(defun luk-elisp-setup ()
  (add-hook 'emacs-lisp-mode-hook 'luk-elisp--hook)
  (define-key emacs-lisp-mode-map (kbd "C-c g") 'xref-find-definitions-other-window)
  (define-key emacs-lisp-mode-map (kbd "<tab>") 'luk-tab-complete-smart-tab)
  (define-key emacs-lisp-mode-map (kbd "M-.") 'luk-elisp-context-hydra))
