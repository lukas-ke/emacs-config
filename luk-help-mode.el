;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'luk-hydra)

(setq luk/help-map (make-sparse-keymap))
(define-key luk/help-map (kbd "q") #'ignore)

(defun luk-help-mode-hydra ()
  "Key-help for help-mode."
  (set-transient-map luk/help-map nil (lambda () (lv-delete-window)))
  (lv-message
   "\
s Goto source
i Goto info page
c Customize
q Close menu"))

(defun luk/on-help-mode ()
  (setq luk-mode-hydra #'luk-help-mode-hydra)
  (setq luk-context-hydra #'luk-help-mode-hydra))

(defun luk-help-mode-setup ()
  (add-hook 'help-mode-hook #'luk/on-help-mode))

(provide 'luk-help-mode)
