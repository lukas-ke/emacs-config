;;; -*- coding: utf-8; lexical-binding: t -*-
(provide 'luk-lsp-conf)

(defun luk-lsp-conf-setup ()
  (define-key lsp-mode-map (kbd "C-c g") 'lsp-find-definition))
