;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'luk-tab-complete)

(defvar luk-magit-started nil "True if magit has been started via luk-magit-status")

(defun luk-magit-status ()
  "Forwards to magit-status, but may also show an init message.

Shows a message the first time it calls magit-status, since
initializing magit takes some time."
  (interactive)
  (when (not luk-magit-started)
    (message "Starting magit..."))
  (setq luk-magit-started t)
  (magit-status)
  (message nil))

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
