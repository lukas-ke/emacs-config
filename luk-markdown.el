;;; -*- coding: utf-8; lexical-binding: t -*-

(provide 'luk-markdown)

(autoload 'markdown-mode ;; https://jblevins.org/projects/markdown-mode/
  "markdown-mode"
  "Major mode for editing Markdown files" t)

(defun luk-markdown--line-wrap-off () (setq truncate-lines t))

(defun luk-markdown-setup ()
  (add-to-list 'auto-mode-alist '("\.md" . markdown-mode))

  ;; Lines can get long (e.g. tables)
  (add-hook 'markdown-mode-hook 'luk-markdown--line-wrap-off))
