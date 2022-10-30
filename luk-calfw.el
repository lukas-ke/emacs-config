;;; luk-calfw.el --- cfw customization     -*- coding: utf-8; lexical-binding: t -*-

;; calfw: Calendar framework
;; Use unicode characters for the table

;;; Commentary:

;; My calfw-calendar configuration

;;; Code:

;; Use unicode box-drawing symbols in the calendar view
(setq cfw:fchar-junction ?╋
      cfw:fchar-vertical-line ?┃
      cfw:fchar-horizontal-line ?━
      cfw:fchar-left-junction ?┣
      cfw:fchar-right-junction ?┫
      cfw:fchar-top-junction ?┯
      cfw:fchar-top-left-corner ?┏
      cfw:fchar-top-right-corner ?┓)

;; I enter the holidays that I care for manually in an org-file
;; (Mainly "Kanelbullens dag").
(setq cfw:display-calendar-holidays nil)

(defun luk-cfw-open-agenda ()
  "Run `cfw:org-open-agenda-day' and refresh the calendar.

This shows the agenda for the selected day side-by-side with the calendar.
The refresh makes the calendar fit its resized window."
  (interactive)
  (let ((calendar-buffer (current-buffer)))
    (cfw:org-open-agenda-day)
    (with-current-buffer calendar-buffer
      (cfw:refresh-calendar-buffer nil))))

(defcustom luk-cfw-date-file-name ""
  "The file name (Note: not full path) of the file for events added via calfw."
  :type 'string)

(defvar luk-is-cfw-capture nil "Set to t when capture is initiated by luk-cfw-capture.

Used to determine if the luk-cfw-upcoming-template needs to
figure out a date on its own or if the date is provided by cfw.
")

(defun luk-cfw-capture ()
  (interactive)
  (setq luk-is-cfw-capture t)
  (cfw:org-capture))

(defun luk-cfw-upcoming-template ()
  "Template for capture of an event via the calendar."
  (if luk-is-cfw-capture
      (progn (setq luk-is-cfw-capture nil) "* %(cfw:org-capture-day) %?")
    (let ((date (org-read-date)))
      (concat "* <" date "> %?"))))

(defun luk-cfw-before-org-capture-finalize ()
  "Refresh calendar when calendar-related capture completes."
  (interactive)
  (when (and luk-cfw-date-file-name
             (string= (buffer-name)
                      (concat "CAPTURE-" luk-cfw-date-file-name)))
      (progn
        (let ((cb (get-buffer "*cfw-calendar*")))
          (when cb
            (with-current-buffer cb
              (cfw:refresh-calendar-buffer nil)))))))

(add-hook 'org-capture-before-finalize-hook #'luk-cfw-before-org-capture-finalize)

(with-eval-after-load 'calfw-org
  (define-key cfw:org-schedule-map (kbd "<return>") #'luk-cfw-open-agenda)
  (define-key cfw:org-schedule-map (kbd "SPC") #'luk-cfw-open-agenda)
  (define-key cfw:org-schedule-map (kbd "<tab>") #'cfw:navi-next-item-command)
  (define-key cfw:org-text-keymap (kbd "<return>") #'cfw:org-onclick)
  (define-key cfw:calendar-mode-map (kbd "a") #'luk-cfw-capture)
  (define-key cfw:calendar-mode-map (kbd "c") #'luk-cfw-capture)
  (define-key cfw:calendar-mode-map (kbd "c") #'luk-cfw-capture))

(defun luk-cfw-show-calendar ()
  (interactive)
  (when (not org-todo-keywords-for-agenda)
    (save-excursion (org-agenda-list)))
  (cfw:open-org-calendar))

(provide 'luk-calfw)

;;; luk-calfw.el ends here
