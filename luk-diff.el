;;; luk-diff.el --- -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;; My configuration for ediff and some utility diff-functions.

;;; Code:

(require 'ediff)
(require 'luk-util)


;; Ediff-hacks (Some of these are probably quite potentially buggy and
;; sensitive to changes in ediff)

(defvar luk--expected-ediff-buffers nil
  "List for buffers that `luk-view-changes' expected ediff to create.

These will be killed on `ediff-quit' by `luk--post-ediff-restore'.

For example, after `ediff-quit', `ediff-current-file' leaves behind
a buffer with the content from the file-on-disk named
\"FILE=<file-path>\".")

(defvar luk--pre-ediff-window-configuration nil
  "Window configuration before running `luk-view-changes'.

This will be restored by `luk--post-ediff-restore'.")

(defun luk-ediff-current-file ()
  "Do some setup and run `ediff-current-file'.

This function:
- stores the window configuration to be able to restore it in
  `luk--post-ediff-restore'.

- adds the expected name of the buffer for the file-content,
  which ediff leaves behind after quit, in
  `luk--expected-ediff-buffers'. Any files in this list are
  killed by `luk--post-ediff-restore'

- makes ediff jump to the first difference."
  (setq luk--pre-ediff-window-configuration (current-window-configuration))
  (add-to-list 'luk--expected-ediff-buffers (concat "FILE=" (buffer-file-name)))
  (ediff-current-file)

  (with-current-buffer "*Ediff Control Panel*" (ediff-next-difference)))

(defun luk--post-ediff-restore (_orig-fun &rest _args)
  "Kill buffers and restore window-configuration after ediff exits.

This function kills the FILE=<path> buffer that ediff doesn't
kill on exit and restores the window configuration to what it was
before running ediff (at least when run through
`luk-view-changes'.

Intended as :after advice for `ediff-really-quit'."
  (when luk--expected-ediff-buffers
    (dolist (buffer luk--expected-ediff-buffers)
      (kill-buffer buffer))
    (setq luk--expected-ediff-buffers nil)
    (when luk--pre-ediff-window-configuration
      (set-window-configuration luk--pre-ediff-window-configuration))))

(defvar luk--original-y-or-n-p (symbol-function #'y-or-n-p)
  "A reference to `y-or-n-p'.

Used to forward to it from `luk--ediff-special-y-or-n-p', after
it has replaced the normal `y-or-n-p'.")

(defun luk--ediff-special-y-or-n-p (prompt)
  "Return t without prompting for quit-ediff prompt.

Other values for PROMPT cause it to forward to the normal
`y-or-n-p'."
  (if (string= prompt "Quit this Ediff session? ")
      t
    (funcall luk--original-y-or-n-p prompt)))

(defun luk--ediff-disable-y-or-n-p (orig-fun &rest args)
  "Disable the quit-ediff prompt.

Used as advice for `ediff-quit' (ORIG-FUN). Forwards ARGS to
ORIG-FUN."
  (cl-letf (((symbol-function 'y-or-n-p) #'luk--ediff-special-y-or-n-p))
    (apply orig-fun args)))


;; Diff functions

(defun luk-buffer-content-same-as-file ()
  "Return t if the buffer and its file has the same content.

Requires that the diff program is available on the path."
  (let ((target-file (make-temp-file "luk-check-diff")))
    (let ((inhibit-message t))
      (write-region (point-min) (point-max) target-file))
    (= 0 (call-process "diff"
                       nil
                       nil
                       nil
                       target-file
                       (buffer-file-name)))))

(defun luk-view-changes ()
  "Show buffer modifications with `ediff-current-file' if buffer is modified.

This provides a pretty nice way to see what has changed since
last save.

If the buffer has been modified but its content matches the file,
this function will also clear the modified flag."
  (interactive)
  (cond
   ((not (buffer-file-name))
    (user-error "Buffer is not associated with a file"))
   ((not (buffer-modified-p))
    ;; Don't try to run ediff if file and buffer identical file
    ;; because it gives a weird "Nothing to revert."-message
    (if (luk-buffer-content-same-as-file)
        (message "File content identical.")
      (luk-ediff-current-file)))
   ((luk-buffer-content-same-as-file)
    ;; Check if the content is indentical, if so just clear the
    ;; modified flag, without starting ediff.
    (message "File content identical — modified flag cleared.")
    (set-buffer-modified-p nil))
   (t (luk-ediff-current-file))))


;; luk-ediff-setup and provide

(defun luk-ediff-setup ()
  "My configuration for `ediff'.

Apart from the plain variable configuration, this includes the
 scary advice-functions `luk--ediff-disable-y-or-n-p' and
 `luk--post-ediff-restore', meant to make `luk-view-changes'
 convenient to use."
  ;; Close unmodified A/B/C buffers when quitting ediff
  (setq ediff-keep-variants nil)

  ;; Horizontal split (side-by-side) makes comparison easier
  (setq ediff-split-window-function #'split-window-horizontally)

  ;; Don't create a frame for the ediff control window
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)

  ;; Disable the ediff exit confirmation
  ;;
  ;; TODO: Would be nice if this was customizable instead in ediff,
  ;; this is probably a bit risky.
  (advice-add 'ediff-quit :around #'luk--ediff-disable-y-or-n-p)

  ;; Restore windows etc. after quitting
  (advice-add 'ediff-really-quit :after #'luk--post-ediff-restore))

(provide 'luk-diff)
;;; luk-diff.el ends here
