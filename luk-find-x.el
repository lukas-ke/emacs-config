;;; luk-find-x.el --- Variants of find-file et. al. -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Hacky alternatives to `find-file' and others with added
;; (hopefully-) transient keymaps that allow cycling between the
;; different file/buffer-finding functions.
;;
;; I occasionally invoke the wrong keybind, say `switch-to-buffer'
;; instead of `find-file', or `luk/find-file-in-git-repo' instead of
;; `find-file'. This lets me switch directly in those cases without
;; first quitting the minibuffer.
;;
;; Also: when switching from one finder to another there is an attempt
;; to retain the entered minibuffer-content when sensible.
;;
;;; Code:
(require 'subr-x)
(require 'recentf)
(require 'luk-util)


;; luk-other
;;
;; TODO: This was probably a bad idea and something I should remove.
;; I'll never remember to switch this properly when it would be useful
;; anyway.

(defvar luk-other nil "When t some operations will target `other-window'.")

(defun toggle-luk-other ()
  "Toggle `luk-other'."
  (interactive)
  (setq luk-other (not luk-other))
  (if luk-other
      (message "Some operations will target other window")
    (message "Some operations will target current window")))


;; luk/quit-and-run

;; Based on ivy-quit-and-run by abo-abo
;; https://emacs.stackexchange.com/a/20980
;; (I couldn't get it to work with keyboard-quit though, used
;; top-level instead)
(defmacro luk/quit-and-run (&rest body)
  "Quit the minibuffer and run BODY afterwards."
  `(progn
       (run-with-idle-timer
        0
        nil
        (lambda ()
          (setq overriding-terminal-local-map nil)
          ,@body))
       (top-level)))


;; recentf-open
;;
;; find-file equivalent for recent files on C-x C-r
;; Uses the enabled completion (ido, ivy, icomplete or fido)
;;
;; As described here:
;; https://www.masteringemacs.org/article/find-files-faster-recent-files-package

(defvar luk/recentf-map (make-sparse-keymap))
(define-key luk/recentf-map (kbd "C-f") (ilambda (luk/quit-and-run (luk-find-file))))
(define-key luk/recentf-map (kbd "C-b")
  (ilambda
   (let ((old-content (minibuffer-contents)))
     (luk/quit-and-run (luk/read-and-switch-to-buffer old-content)))))

(declare-function ivy-completing-read nil)

(defun luk-recentf-open (&optional initial-input)
  "Use a `completing-read' function to select and open a recent file.

This function checks for some enabled completion-framework
modes (like `ido-mode') or falls back on `completing-read'
\(possibly with `fido-mode' which is pretty nice). Optional
argument INITIAL-INPUT If INITIAL-INPUT is specified, it will be
the initial-input for the `completing-read'."
  (interactive)

  (let ((exit-map (set-transient-map luk/recentf-map (lambda () t))))
    (condition-case nil
        (let ((completer (cond ((bound-and-true-p ido-mode) #'ido-completing-read)
                               ((bound-and-true-p ivy-mode) #'ivy-completing-read)
                               (t #'completing-read))))
          (find-file (funcall completer "Find recent file: " recentf-list nil nil initial-input))
          (funcall exit-map))
      (quit (funcall exit-map)))))


;; switch-to-buffer

(defvar luk/switch-buffer-map (make-sparse-keymap))

(define-key luk/switch-buffer-map (kbd "C-r")
  (ilambda
   (let ((old-content (minibuffer-contents)))
     (luk/quit-and-run (luk-recentf-open old-content)))))

(define-key luk/switch-buffer-map (kbd "C-f")
  (ilambda
   (luk/quit-and-run (luk-find-file))))

(defun luk/read-and-switch-to-buffer (&optional initial-input)
  "Read buffer to switch to, return buffer name as a string.

Start with INITIAL-INPUT entered when specified.

This function is only for interactive use."
  (interactive)
  ;; TODO: indicate default

  (let ((exit-map (set-transient-map luk/switch-buffer-map (lambda () t))))
    (condition-case nil
        (let ((buf (completing-read "Switch to buffer: " (internal-complete-buffer-except) nil t initial-input)))
          (if (or luk-other
                  (window-dedicated-p)
                  (string-prefix-p (buffer-name) "CAPTURE-"))

              (switch-to-buffer-other-window buf)
            (switch-to-buffer buf)))
      (quit (funcall exit-map)))))


;; find-file
(defvar luk/find-file-map (make-sparse-keymap))

(define-key luk/find-file-map (kbd "C-r")
  (ilambda
   (let ((old-content (minibuffer-contents)))
     (luk/quit-and-run (luk-recentf-open old-content)))))

(define-key luk/find-file-map (kbd "C-b")
  (ilambda
   (let ((old-content (string-remove-prefix default-directory (minibuffer-contents))))
     (luk/quit-and-run (luk/read-and-switch-to-buffer old-content)))))

(define-key luk/find-file-map (kbd "C-f")
  (ilambda
   (let ((repo-dir (luk-find-git-repo default-directory 'noerror)))
     (when (not repo-dir)
       (user-error "Not in a git repository"))
     (let ((old-content
            ;; Strip the repo-dir if present, since find-file-in-git-repo is
            ;; relative to repo root
            (string-remove-prefix repo-dir (minibuffer-contents))))
       (luk/quit-and-run (luk-find-file-in-git-repo old-content))))))

(defun luk-find-file ()
  ;; TODO: Add initial-input (would require custom find-file though
  "Like regular `find-file' but with some silly hacks."
  (interactive)
  (let ((exit-map (set-transient-map luk/find-file-map (lambda () t)))
        (use-dialog-box nil))
    (condition-case nil
        (progn
          (if luk-other
              (call-interactively #'find-file-other-window)
            (call-interactively #'find-file))
          (funcall exit-map))
      (quit (funcall exit-map)))))


;; luk-find-file-in-git-repo
(defvar luk/find-in-repo-map (make-sparse-keymap))

(define-key luk/find-in-repo-map (kbd "C-r")
  (ilambda
   (let ((old-content (minibuffer-contents)))
     (luk/quit-and-run (luk-recentf-open old-content)))))

(define-key luk/find-in-repo-map (kbd "C-b")
  (ilambda
   (let ((old-content (minibuffer-contents)))
     (luk/quit-and-run (luk/read-and-switch-to-buffer old-content)))))

(define-key luk/find-in-repo-map (kbd "C-f")
  (ilambda
   (luk/quit-and-run (luk-find-file))))

(defun luk-find-file-in-git-repo (&optional initial-input)
  "Find files in the current repository with completion.

When INITIAL-INPUT is provided, it is entered initially for
`completing-read'."
  (interactive)
  (let ((default-directory (luk-find-git-repo default-directory)))
    (let ((exit-map (set-transient-map luk/find-in-repo-map (lambda () t))))
      (condition-case nil
          (find-file
           (completing-read
            "Find repository file: "
            (butlast (split-string (shell-command-to-string "git ls-files") "\n"))
            nil nil initial-input))
        (quit (funcall exit-map))))))


(provide 'luk-find-x)

;;; luk-find-x.el ends here
