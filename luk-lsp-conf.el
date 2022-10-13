;;; -*- coding: utf-8; lexical-binding: t -*-
(require 'luk-hydra)

(defun luk-lsp-except-in-org ()
  (when (buffer-file-name)
    (lsp)))

;; WIP
(defhydra luk-lsp-context-hydra (:hint nil :foreign-keys warn :exit nil)
  "
_g_: Find definition
_f_: Find references
_h_: Display info/help
_q_: Exit
"
  ("g" xref-find-definitions-other-window :exit t)
  ("f" lsp-find-references :exit t)
  ("h" lsp-describe-thing-at-point :exit t)
  ("q" nil :exit t))

(defun luk-lsp-headerline-breadcrumb-mode ()
  ;; The file that defines the symbol lsp-headline-breadcrumb-mode
  ;; might not have been loaded yet, but gets autoloaded when the
  ;; function lsp-headerline-breadcrumb-mode is invoked by the hydra.
  (and (boundp 'lsp-headerline-breadcrumb-mode) lsp-headerline-breadcrumb-mode))

(defhydra luk-lsp-file-hydra (:hint nil :foreign-keys warn :exit nil)
  "
Show^^                     Toggle
_o_: File symbols          _h_: Symbol highlight: %s`lsp-enable-symbol-highlighting
_e_: Flycheck errors       _b_: Breadcrumb:       %s(luk-lsp-headerline-breadcrumb-mode)
_S_: LSP Session           _c_: Doc for point:    %s`lsp-ui-doc-show-with-cursor
^ ^                        _s_: Sideline info:    %s`lsp-ui-sideline-mode
_f_: Focus doc frame

_q_: Exit
"
  ("." (luk-hydra-push #'luk-lsp-file-hydra/body "lsp") :exit t)
  ("o" lsp-ui-imenu :exit t)
  ("e" flycheck-list-errors :exit t)
  ("f" lsp-ui-doc-focus-frame :exit t)
  ("S" #'lsp-describe-session :exit t)
  ("h" lsp-toggle-symbol-highlight)
  ("b" (lsp-headerline-breadcrumb-mode 'toggle))
  ("c" (progn (setq lsp-ui-doc-show-with-cursor (not lsp-ui-doc-show-with-cursor)) (when (not lsp-ui-doc-show-with-cursor) (lsp-ui-doc-hide))))
  ("s" (lsp-ui-sideline-mode 'toggle))

  ("q" nil :exit t))



(defun luk-lsp-hook ()
  'yas-minor-mode
  (define-key lsp-mode-map (kbd "M-.") 'luk-lsp-file-hydra/body)
  (define-key lsp-mode-map (kbd "M-,") 'luk-lsp-context-hydra/body) ;; WIP
  ;; TODO: Consider if region active etc., e.g. comment/uncomment region
  )

(defun luk-lsp-conf-setup ()
  (define-key lsp-mode-map (kbd "C-c g") 'xref-find-definitions-other-window)

  ;; lsp-mode requires yas-snippets for completion
  ;; (Otherwise I get $0 when completing calls with arguments, e.g.
  ;; some_func($0)|
  (add-hook 'lsp-mode-hook 'luk-lsp-hook)
  (add-hook 'python-mode-hook #'luk-lsp-except-in-org)

  ;; Disable the File > Class > Function header.
  ;; It's a little useful, but also kinda wasteful.
  (setq lsp-headerline-breadcrumb-enable nil)

  ;; Show the documentation popup when point is at something.
  ;; This might drive me nuts, but I don't use the mouse.
  ;;
  ;; Maybe there's a better way to do this, e.g. some simple keypress.
  (setq lsp-ui-doc-show-with-cursor t))

(provide 'luk-lsp-conf)
