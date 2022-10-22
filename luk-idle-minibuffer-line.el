;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'minibuffer-line)
(require 'appt)

(defun luk-get-now-mins ()
  "Return appt-style minute for the present moment."
  (let ((now (decode-time)))
    (+ (* 60 (decoded-time-hour now)) (decoded-time-minute now))))

(defun luk-minutes-until-appt (appt)
  "Return the number of minutes until APPT.

APPT is an appointment entry from the `appt-time-msg-list'."
  (- (car (car appt)) (luk-get-now-mins)))

(defun luk-get-next-appt ()
  "Gets the next appointment."
  (car appt-time-msg-list))

(defun luk-next-appt-str ()
  "Get a string for the next appt-appointment from ‘appt-time-msg-list’."
  (let ((next-appt (luk-get-next-appt)))
    (if (not next-appt)
        ""
      (let ((minutes-left (luk-minutes-until-appt next-appt)))
        (if (< minutes-left 10)
            (propertize (format "Next appt: %s (in %d min.)" (car (cdr next-appt)) minutes-left)
                        'face '(:weight bold))
          (format "Next appt: %s" (car (cdr next-appt))))))))

(defun luk-clear-to-idle-minibuffer-line ()
  "Clear the minibuffer message and restore display of `minibuffer-line--buffer'.

For use with the `clear-message-function'
This function restores the content managed by
`minibuffer-line-mode' immediately, without waiting for the
`minibuffer-line--timer'.

For example in `emacs-lisp-mode' with `eldoc-mode', documentation
is shown in the echo-area for symbols at point. When point moves
out, the messages are cleared. If the normal
`clear-minibuffer-message' is used, this will leave the
minibuffer area blank until the \"minibuffer-line\" is
redisplayed via it's timer, while this function instead restores
the display to the current minibuffer-line content, without
updating it - that is still done by the timer)"
  (clear-minibuffer-message)
  (with-current-buffer minibuffer-line--buffer
    ;; *Somehow* modifying the `minibuffer-line--buffer' causes it to be
    ;; displayed in the minibuffer area. I could call minibuffer-line--update,
    ;; but that's more expensive.
    ;; TODO: There has to be a better way to redisplay the buffer
    ;; than inserting and deleting a character
    (goto-char 0)
    (insert "x")
    (goto-char 0)
    (delete-char 1)))

(defun luk-idle-minibuffer-line-enable ()
  "Display time and appointments in minibuffer."
  (interactive)
  (setq appt-display-mode-line nil)
  (setq
   minibuffer-line-format
   '((:eval
      (let ((time-string (format-time-string "%H:%M %b %d %a"))
            (next-appt-str (luk-next-appt-str)))
        (concat
         next-appt-str

         ;; Right-side-adjusted clock string
         (make-string (- (frame-text-cols)
                         (string-width next-appt-str)
                         (string-width time-string)
                         1) ? )
         time-string)))))
  (face-spec-set 'minibuffer-line '((t :inherit default)))
  (setq clear-message-function #'luk-clear-to-idle-minibuffer-line)
  (minibuffer-line-mode))

(provide 'luk-idle-minibuffer-line)
