;;; -*- coding: utf-8; lexical-binding: t -*-

(provide 'luk-dired)
(require 'dired)

(defun luk-dired--sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defun luk-dired-setup ()
  (defadvice dired-readin
      (after dired-after-updating-hook first () activate)
    "Sort dired listings with directories first before adding marks."
    (luk-dired--sort))

  ;; Only show names in dired, not sizes, permissions
  (add-hook 'dired-before-readin-hook 'dired-hide-details-mode)

  ;; Use M-<up> to go up one dir, like in Windows explorer.
  (define-key dired-mode-map (kbd "M-<up>") 'dired-up-directory)

  (add-hook 'dired-mode-hook 'auto-revert-mode))
