;;; -*- coding: utf-8; lexical-binding: t -*-

(provide 'luk-magit)

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
