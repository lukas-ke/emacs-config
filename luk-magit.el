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


;;Rebase heading

(defun luk-magit-rebase-show-rebase-heading ()
  "Indicate the reversed commit-order in header in rebase-todo.

This can get a little confusing with magit, as you get to the rebase
file after picking the end commit in the normal log-order."
  (setq header-line-format
        ;; Adjust for margins (e.g. with perfect-margin-mode)
        '((:eval
           (let ((pad (car (window-margins))))
             (concat (if pad (make-string pad ? ) "") " Magit rebase (oldest commit first)"))))))

(add-hook 'git-rebase-mode-hook #'luk-magit-rebase-show-rebase-heading)


;; Stop `luk-tab-complete' from ruining cycling in the magit status
;; buffer.

(defun luk-magit-tab-cycle-heading ()
  "Function for `luk-tab-complete-custom'"
  (interactive)
  (call-interactively #'magit-section-toggle)
  t)

(defun luk--on-magit-mode ()
  (setq luk-tab-complete-custom #'luk-magit-tab-cycle-heading))

(add-hook 'magit-mode-hook #'luk--on-magit-mode)

;; Commit hydra and friends

(defun luk-diff-while-committing ()
  "Show the diff for the worked-on commit without asking to save.

As far as I know, saving files won't have any effect on the diff
for the ongoing commit."
  (interactive)
  (let ((magit-save-repository-buffers nil))
    (magit-diff-while-committing)))

(defhydra luk-git-commit-hydra (:hint nil
                               :foreign-keys warn
                               :pre (setq hydra-amaranth-warn-message "Invalid key.")
                               :post (setq hydra-amaranth-warn-message luk-hydra-amaranth-original-message))
  (format "\
Main ➤ %s      _._: up
^─^──────────────────────────
_s_ Save message
_p_ Previous message
_n_ Next message
_d_ Show diff

_q_ Close menu
"
          (luk-caption "Commit"))
  ("." (luk-hydra-push 'luk-git-commit-hydra/body "git-commit") :exit t)
  ("s" (git-commit-save-message) :exit t)
  ("p" (git-commit-next-message 1) :exit nil)
  ("n" (git-commit-next-message 1) :exit nil)
  ("d" (luk-diff-while-committing) :exit t)
  ("q" nil :exit t))


(with-eval-after-load 'magit
  ;; Alternative to q for revision buffers (blobs)
  (define-key magit-blob-mode-map (kbd "k") #'magit-kill-this-buffer)

  ;; Hydra for commit messages
  (define-key git-commit-mode-map (kbd "M-.") #'luk-git-commit-hydra/body))

(provide 'luk-magit)
