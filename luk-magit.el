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
                               :pre (setq hydra-amaranth-warn-message "Invalid key (git commit hydra)")
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


;; SMerge hydra

;; Stolen from Adam Porter (and modified).
;; https://github.com/alphapapa/unpackaged.el/blob/master/unpackaged.el
(defhydra luk-smerge-hydra
  (:hint nil
         :foreign-keys warn
         :pre (setq hydra-amaranth-warn-message "Invalid key (SMerge hydra)")
         :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :exit t)
    ("." (luk-hydra-push 'luk-smerge-hydra/body "smerge") "Up" :exit t)
    ("q" nil "cancel" :exit t))


(with-eval-after-load 'magit
  ;; Alternative to q for revision buffers (blobs)
  (define-key magit-blob-mode-map (kbd "k") #'magit-kill-this-buffer))

(add-hook 'smerge-mode-hook (lambda () (setq luk-mode-hydra #'luk-smerge-hydra/body)))

(with-eval-after-load 'vc-git
  ;; For `vc-log-outgoing' and related. This is faster for skimming
  ;; the log than the magit-log which takes some time to reveal each
  ;; entry.

  ;; Use tab to cycle an entry, like how magit cycles sections
  (define-key vc-git-log-view-mode-map (kbd "<tab>") #'log-view-toggle-entry-display)
  (define-key vc-git-log-view-mode-map (kbd "TAB") #'log-view-toggle-entry-display)

  ;; This was scroll-up otherwise, maybe I'll miss it, but in magit
  ;; <space> shows the log without changing focus, so this seems more
  ;; consistent
  (define-key vc-git-log-view-mode-map (kbd "<space>") #'log-view-toggle-entry-display))


;; Tweaks for commit-message editing
(defun luk-commit-setup ()
  (setq luk-mode-hydra #'luk-git-commit-hydra/body))

(with-eval-after-load 'git-commit
  (add-hook 'git-commit-setup-hook #'luk-commit-setup)
  (when (require 'orgalist nil 'noerror)
    (add-hook 'git-commit-setup-hook #'orgalist-mode)))



(provide 'luk-magit)
