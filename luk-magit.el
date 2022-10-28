;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'luk-tab-complete)

(defun luk-magit-status-set-key (key)
  "Globally bind a key to start magit and run magit-status

Shows a message the first time it calls magit-status, since
initializing magit takes some time."
  (global-set-key
   key
   (lambda()
     (interactive)
     (message "Starting magit...")
     (magit-status)
     (global-set-key key 'magit-status))))

(defun luk-magit-rebase-show-rebase-heading ()
  (setq header-line-format "Magit rebase (oldest commit first)"))

(add-hook 'git-rebase-mode-hook #'luk-magit-rebase-show-rebase-heading)

(defun luk-magit-tab-cycle-heading ()
  (interactive)
  (call-interactively #'magit-section-toggle)
  t)

(defun luk--on-magit-mode ()
  (setq luk-tab-complete-custom #'luk-magit-tab-cycle-heading))

(add-hook 'magit-mode-hook #'luk--on-magit-mode)

(provide 'luk-magit)
