;;; -*- coding: utf-8; lexical-binding: t -*-
(require 'appt)
(require 'luk-hydra)

;; TODO: Add ert-test

(defface luk-appt-popup '((t :inherit default))
  "Face for the child-frame background used for appt.")

(defface luk-appt-popup-border '((t :inherit default))
  "Face defining the border color of the child-frame used for appt notifications.

The background attribute is used for the border.")


;; Functions for displaying appointments in a frame

(defun luk-appt-get-frame (name)
  "Get a frame by name.

Modified from built-in `select-frame-by-name'."
  (interactive
   (let* ((frame-names-alist (make-frame-names-alist))
	   (default (car (car frame-names-alist)))
	   (input (completing-read
		   (format-prompt "Select Frame" default)
		   frame-names-alist nil t nil 'frame-name-history)))
     (if (= (length input) 0)
	 (list default)
       (list input))))

  (or (cdr (assoc name (make-frame-names-alist)))
      (catch 'done
        (dolist (frame (frame-list))
          (when (equal (frame-parameter frame 'name) name)
            (throw 'done frame))))
      (error "There is no frame named `%s'" name)))

(defun luk-show-frame (text text-width)
  "Display text in a child frame."
  (with-current-buffer (get-buffer-create " appt-notification")
    (erase-buffer)
    (insert text)
    (make-local-variable 'mode-line-format)
    (setq mode-line-format nil)
    (setq truncate-lines t)
    (setq cursor-type nil)
    (goto-char (point-min)))
  (let ((display-buffer-alist
         `((" appt-notification" display-buffer-in-child-frame
            (dedicated . nil)
            (child-frame-parameters
             (undecorated . t)
             (left . 100)
             (name . "luk-child-frame")
             (width . ,text-width)
             (height . 6) ;; TODO: Get from appt-notification buffer
             (minibuffer)
             (mode-line nil)
             (left-fringe . 0)
             (right-fringe . 0)
             (child-frame-border-width . 2)
             (background-color . ,(face-background 'luk-appt-popup)))))))
      (display-buffer " appt-notification")
      (set-face-attribute 'child-frame-border (luk-appt-get-frame "luk-child-frame") :background (face-background 'luk-appt-popup-border))))

(defun luk-appt-delete-frame ()
    (interactive)
    (delete-frame (luk-appt-get-frame "luk-child-frame")))

(defun luk-appt-display-frame (minutes time-at-notification appointment-text)
  (let ((additional-str ""))
    (when (listp minutes)
      ;; Report the first element when notified for multiple meetings,
      ;; and show a note that there are more pending meetings.

      (cl-assert (not (listp time-at-notification))) ;; This element is not a list, even when the others are
      (setq additional-str (format "\n + %d %s"
                                   (- (length minutes) 1)
                                   (if (>(length minutes) 2) "appointments" "appointment")))
      (setq minutes (car minutes))
      (setq appointment-text (car appointment-text)))
    (let* ((time-left-str
            (let ((n-minutes (string-to-number minutes)))
              (cond
               ((> n-minutes 1)
                (format "\n In %s minutes\n" minutes))
               ((= n-minutes 1)
                (format "\n In 1 minute\n" minutes))
               (t "\n NOW"))))
           (meeting-str (propertize appointment-text 'face '(:height 1.5)))
           (text (concat time-left-str "\n " meeting-str additional-str)))
      (luk-show-frame text (round (* 2 (+ (length meeting-str) (length additional-str))))))))


;; appt ignore functions, for ignoring meetings across subsequent
;; exports with e.g. org-agenda-to-appt

(defvar luk-appt-ignored nil
  "Ignored entries from appt-time-msg-list.

The entries are kept in this variable so that the ignored state
persists even if the appointments are recreated, for example with
`org-agenda-to-appt'.")

(defvar luk-appt-manually-added nil
  "Entries added with `luk-appt-add-manually'.

The entries are kept in this variable so that they can be
reinsterted after the `appt-time-msg-list' is recreated (by for
example `org-agenda-to-appt'")

(defun luk-appt-unignore-all ()
  "Clear the list of ignored apps"
  (interactive)
  (setq luk-appt-ignored nil)
  (message (concat "Ignore list cleared. Note: re-export to restore appointments.")))

;; TODO: Generalize the luk-appt-list functions
(defun luk-appt-list-pending ()
  (interactive)
  (message (concat (luk-caption "Appointments")
                   (if (= 0 (length appt-time-msg-list)) " None"
                     (concat (if (= 1 (length appt-time-msg-list)) " " "\n")
                             (string-join (mapcar (lambda (x) (substring-no-properties (cadr x))) appt-time-msg-list) "\n"))))))

;; TODO: Entries should be cleared when in the past
(defun luk-appt-list-ignored ()
  (interactive)
  (message
   (concat
    (luk-caption "Ignored appointments")
    (if (= 0 (length luk-appt-ignored))
        " None"
      (concat
       (if (= 1 (length luk-appt-ignored)) " " "\n")
       (string-join
        (mapcar (lambda (x) (substring-no-properties (cadr x))) luk-appt-ignored)
        "\n"))))))

(defun luk-appt-list-manually-added ()
  (interactive)
  (message
   (concat
    (luk-caption "Manually added appointments")
    (if (= 0 (length luk-appt-manually-added))
        " None"
      (concat
       (if (= 1 (length luk-appt-manually-added)) " " "\n")
       (string-join
        (mapcar (lambda (x) (substring-no-properties (cadr x))) luk-appt-manually-added)
        "\n"))))))

(defun luk-appt-ignore-next ()
  "Ignore the next upcoming appointment"
  (interactive)
  (let ((element (car appt-time-msg-list)))
    (when (not element)
      (user-error "No next appointment to ignore"))
    (add-to-list 'luk-appt-ignored (car appt-time-msg-list))
    (setq luk-appt-ignored (appt-sort-list luk-appt-ignored))
    (luk-filter-appt)))

(defun luk-appt-next-str () ;; Todo almost duplicates luk-idle-minibuffer-line
  (let ((next-appt (cadr (cadr appt-time-msg-list))))
    (if next-appt
        (substring-no-properties next-appt nil (min 27 (length next-appt)))
      "")))

;; TODO: Sort ignored (appt-sort-list)
(defun luk-appt-ignore-some ()
  "Ignore some appointments."
  (interactive)
  (when (= 0 (length appt-time-msg-list))
    (user-error "No pending appointments"))
  (let ((tmp-msg-list appt-time-msg-list)
        (num-ignored 0))
    (dolist (element tmp-msg-list)
      (when (y-or-n-p (concat "Ignore "
                            (prin1-to-string
                             (substring-no-properties (cadr element) 0))
                            "? "))
        ;; TODO: Need a better check, to not add twice
        ;; eq doesn't work
        (add-to-list 'luk-appt-ignored element)
        (setq num-ignored (+ num-ignored 1))))
    (luk-filter-appt)
    (appt-check)
    (message "Ignored %d appointments." num-ignored)))

(defun luk-appt-is-ignored (appt)
  (dolist (el luk-appt-ignored)
    (when (= (caar el) (caar appt))
      (cl-return t))))

(defun luk-filter-appt ()
  "Remove ignored appointments from `appt-time-msg-list'."
  (interactive)

  ;; Convert current time to minutes after midnight (12:01am = 1),
  ;; and remove ignored items that are in the past.
  (let* ((now (decode-time))
         (now-mins (+ (* 60 (decoded-time-hour now))
                      (decoded-time-minute now))))
    (while (and luk-appt-ignored
                (< (caar (car luk-appt-ignored)) now-mins))
      (setq luk-appt-ignored (cdr luk-appt-ignored))))

  (let ((n-before (length appt-time-msg-list)))
    (setq appt-time-msg-list
          (remq nil
                (mapcar (lambda (appt) (if (luk-appt-is-ignored appt) nil appt))
                        appt-time-msg-list)))
    (let ((n-after (length appt-time-msg-list)))
      (message "%d pending appointments (%d ignored)." n-after (- n-before n-after)))))

(defun luk-appt-insert-manually-added ()
  "Add retained manually added appointments to the `appt-time-msg-list'."

  ;; Convert current time to minutes after midnight (12:01am = 1),
  ;; and remove elements in the list that are in the past.
  (let* ((now (decode-time))
         (now-mins (+ (* 60 (decoded-time-hour now))
                      (decoded-time-minute now))))
    (while (and luk-appt-manually-added
                (< (caar (car luk-appt-manually-added)) now-mins))
      (setq luk-appt-manually-added (cdr luk-appt-manually-added))))
  (dolist (time-msg luk-appt-manually-added)
    (add-to-list 'appt-time-msg-list time-msg))
  (setq appt-time-msg-list (appt-sort-list appt-time-msg-list)))

(defun luk-export-agenda-filtered ()
  "Export agenda to appt, but filter ignored appointments."
  (interactive)
  (setq appt-time-msg-list nil)
  (appt-activate 0)
  ;; TODO: Fishy dependency. Maybe better to put this in org, and
  ;; depend on luk-appt, calling some plain function in luk-appt to
  ;; filter, or passing the update function (in this case
  ;; (org-agenda-to-appt)) as an arg
  (org-agenda-to-appt)
  (luk-filter-appt)
  (luk-appt-insert-manually-added)
  (appt-activate 1))

(defun luk-appt-toggle ()
  (interactive)
  (if appt-timer
      (appt-activate -1)
    (appt-activate 1)))

(defun luk-appt-add-manually (time msg &optional warntime)
  "Add appointment interactively with extra retention.

Add an appointment using `appt-add', and also store it in
`luk-appt-manually-added'. This is useful to add just some quick
reminder, without having to add it to org files or diary."
  (interactive "sTime (hh:mm[am/pm]): \nsMessage: \n\
sMinutes before the appointment to start warning: ")
  (unless (string-match appt-time-regexp time)
    (user-error "Unacceptable time-string"))
  (and (stringp warntime)
       (setq warntime (unless (string-equal warntime "")
                        (string-to-number warntime))))
  (and warntime
       (not (integerp warntime))
       (user-error "Argument WARNTIME must be an integer, or nil"))
  (or appt-timer (appt-activate))
  (let ((time-msg (list (list (appt-convert-time time))
                        (concat time " " msg) t)))
    (when warntime (setq time-msg (append time-msg (list warntime))))

    ;; Retain manually added times in a separate list
    (add-to-list 'luk-appt-manually-added time-msg)
    (setq luk-appt-manually-added
          (appt-sort-list (nconc luk-appt-manually-added (list time-msg))))

    (unless (member time-msg appt-time-msg-list)
      (setq appt-time-msg-list
            (appt-sort-list (nconc appt-time-msg-list (list time-msg)))))))


;; Appt hydra
;; TODO: Customize the face of luk-appt-next-str to make it look less
;; like a menu option
;; TODO: Don't insert luk-appt-next-str line if blank
(defhydra luk-appt-hydra (:hint nil :foreign-keys warn
                                :pre (setq hydra-amaranth-warn-message "Invalid key for luk-appt-setup.")
                                :post (setq hydra-amaranth-warn-message luk-hydra-amaranth-original-message))
  (format "\
%s
_a_   Add appointment             _n_ %%s(luk-hydra-checkbox appt-timer) Notification
_x_   Ignore next                 Notify %%s`appt-message-warning-time minutes in advance, then every %%s`appt-display-interval minutes.
    %%-29s(luk-appt-next-str)
_s_   Ignore some
_l p_ List pending
_l i_ List ignored
_l m_ List manually added
_c_ Check now
_C_ Unignore all

_q_ Quit"
          (luk-caption "Appointments"))
  ("n" #'luk-appt-toggle :exit nil)
  ("x" (luk-appt-ignore-next) :exit t)
  ("s" (luk-appt-ignore-some) :exit t)
  ("l p" (luk-appt-list-pending) :exit t)
  ("l i" (luk-appt-list-ignored) :exit t)
  ("l m" (luk-appt-list-manually-added) :exit t)
  ("a" (call-interactively #'luk-appt-add-manually) :exit t)
  ("c" (appt-check) :exit t)
  ("C" (luk-appt-unignore-all) :exit t)
  ("q" nil :exit t))


(defun luk-appt-setup ()
  "Use `luk-appt-display-frame' with `appt'."
  (interactive)
  (setq appt-disp-window-function #'luk-appt-display-frame)
  (setq appt-delete-window-function #'luk-appt-delete-frame))

(provide 'luk-appt)
