;;; -*- coding: utf-8; lexical-binding: t -*-

;;; Wrap-around with next-error
;; Modified from https://stackoverflow.com/a/21161239

(provide 'luk-next-error-cycle)

(defvar luk-modeline-flash-color "#af00d7")

(defun luk-indicate-error-nav-wrapped (direction)
  "Display a message in minibuffer indicating that we wrapped
also flash the mode-line"
  (let ((mode-line-color (face-background 'mode-line)))
    (message "Wrapped %s error" (symbol-name direction))
    (set-face-background 'mode-line luk-modeline-flash-color)
    (sit-for 0.3)
    (set-face-background 'mode-line mode-line-color)))

(defun luk-next-error-cycle (&optional arg reset)
  "Go to the next error, cycle to first if at the end.

If in flymake-mode, use `flymake-goto-next-error'. It already has
the cycle-behavior.

If in any other mode, use `next-error', which goes to the next
compilation error in any buffer.

Prefix argument ARG says how many error messages to move
forwards (or backwards, if negative). With just C-u as prefix
moves to first error"
  (interactive "P")
  (if (luk--in-flymake-mode)
      (flymake-goto-next-error)
    (condition-case nil
        (call-interactively 'next-error)
      ('user-error (progn (next-error 1 t)
                          (luk-indicate-error-nav-wrapped 'next))))))

(defun luk-jump-to-last-error (buffer)
  "Jump to last error in the BUFFER, this assumes that
the error is at last but third line"
  (save-selected-window
    (select-window (get-buffer-window buffer))
    (goto-char (point-max))
    (forward-line -3)
    (call-interactively 'compile-goto-error)))

(defun luk-previous-error-cycle (&optional arg)
  "Jumps to previous error if at first error jump to last error instead.
Prefix argument ARG says how many error messages to move backwards (or
forwards, if negative)."
  (interactive "P")
  (condition-case nil
      (if (compilation-buffer-p (current-buffer))
          (compilation-previous-error 1)
        (call-interactively 'previous-error))
    ('user-error (progn
                   (let ((error-buffer (next-error-find-buffer)))
                     ;; If the buffer has an associated error buffer use it to
                     ;; to move to last error
                     (if (and (not (eq (current-buffer) error-buffer))
                              (compilation-buffer-p error-buffer))
                         (luk-jump-to-last-error error-buffer)
                       ;; Otherwise move to last point and invoke previous error
                       (goto-char (point-max))
                       (call-interactively 'previous-error))
                     (luk-indicate-error-nav-wrapped 'previous))))))
