;;; -*- coding: utf-8; lexical-binding: t -*-
(provide 'luk-lsp-conf)

(defun luk-lsp-conf-setup ()
  (define-key lsp-mode-map (kbd "C-c g") 'xref-find-definitions-other-window)

  ;; lsp-mode requires yas-snippets for completion
  ;; (Otherwise I get $0 when completing calls with arguments, e.g.
  ;; some_func($0)|
  (add-hook 'lsp-mode-hook 'yas-minor-mode)
  (add-hook 'python-mode-hook 'lsp)

  ;; Disable the File > Class > Function header.
  ;; It's a little useful, but also kinda wasteful.
  (setq lsp-headerline-breadcrumb-enable nil)

  ;; Show the documentation popup when point is at something.
  ;; This might drive me nuts, but I don't use the mouse.
  ;;
  ;; Maybe there's a better way to do this, e.g. some simple keypress.
  (setq lsp-ui-doc-show-with-cursor t))
