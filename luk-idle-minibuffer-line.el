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
  (minibuffer-line-mode))

(provide 'luk-idle-minibuffer-line)
