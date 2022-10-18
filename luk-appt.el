;;; -*- coding: utf-8; lexical-binding: t -*-
(require 'appt)
(require 'hydra)

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


;; appt ignore functions, for ignoring meetings across. subsequent
;; exports with e.g. org-agenda-to-appt

(defvar luk-ignored-appts nil
  "Ignored entries from appt-time-msg-list.

The entries are kept in this variable so that the ignored state
persists even if the appointments are recreated, for example with
`org-agenda-to-appt'.")

(defun luk-appt-unignore-all ()
  "Clear the list of ignored apps"
  (interactive)
  (setq luk-ignored-appts nil)
  (message (concat "Ignore list cleared. Note: re-export to restore appointments.")))

(defun luk-appt-list ()
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
    (if (= 0 (length luk-ignored-appts))
        " None"
      (concat
       (if (= 1 (length luk-ignored-appts)) " " "\n")
       (string-join
        (mapcar (lambda (x) (substring-no-properties (cadr x))) luk-ignored-appts)
        "\n"))))))

(defun luk-appt-ignore-next ()
  "Ignore the next upcoming appointment"
  (interactive)
  (let ((element (car appt-time-msg-list)))
    (when (not element)
      (user-error "No next appointment to ignore"))
    (add-to-list 'luk-ignored-appts (car appt-time-msg-list))
    (luk-filter-appt)))

(defun luk-appt-next-str () ;; Todo almost duplicates luk-idle-minibuffer-line
  (let ((next-appt (car appt-time-msg-list)))
    (if (not next-appt)
        ""
      (substring-no-properties (car (cdr next-appt)) nil 27))))

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
        (add-to-list 'luk-ignored-appts element)
        (setq num-ignored (+ num-ignored 1))))
    (luk-filter-appt)
    (appt-check)
    (message "Ignored %d appointments." num-ignored)))

(defun luk-appt-is-ignored (appt)
  (dolist (el luk-ignored-appts)
    (when (= (caar el) (caar appt))
      (cl-return t))))

(defun luk-filter-appt ()
  "Remove ignored appointments from `appt-time-msg-list'."
  (interactive)
  (let ((n-before (length appt-time-msg-list)))
    (setq appt-time-msg-list
          (remq nil
                (mapcar (lambda (appt) (if (luk-appt-is-ignored appt) nil appt))
                        appt-time-msg-list)))
    (let ((n-after (length appt-time-msg-list)))
      (message "%d pending appointments (%d ignored)." n-after (- n-before n-after)))))

(defun luk-export-agenda-filtered ()
  "Export agenda to appt, but filter ignored appointments."
  (interactive)
  (setq appt-time-msg-list nil)
  (appt-activate 0)
  (org-agenda-to-appt)
  (luk-filter-appt)
  (appt-activate 1))

(defun luk-appt-toggle ()
  (interactive)
  (if appt-timer
      (appt-activate -1)
    (appt-activate 1)))


;; Appt hydra
(defhydra luk-appt-hydra (:hint nil :foreign-keys warn
                                :pre (setq hydra-amaranth-warn-message "Invalid key for luk-appt-setup.")
                                :post (setq hydra-amaranth-warn-message luk-hydra-amaranth-original-message))
  (format "\
%s
_n_ Ignore next                 _a_ %%s(luk-hydra-checkbox appt-timer) Notification
  %%-29s(luk-appt-next-str)     Notify %%s`appt-message-warning-time minutes in advance, then every %%s`appt-display-interval minutes.
_s_ Ignore some
_l_ List
_L_ List ignored
_c_ Check now
_C_ Unignore all

_q_ Quit"
          (luk-caption "Appointments"))q
  ("a" #'luk-appt-toggle :exit nil)
  ("n" (luk-appt-ignore-next) :exit t)
  ("s" (luk-appt-ignore-some) :exit t)
  ("l" (luk-appt-list) :exit t)
  ("L" (luk-appt-list-ignored) :exit t)
  ("c" (appt-check) :exit t)
  ("C" (luk-appt-unignore-all) :exit t)
  ("q" nil :exit t))


(defun luk-appt-setup ()
  "Use `luk-appt-display-frame' with `appt'."
  (interactive)
  (setq appt-disp-window-function #'luk-appt-display-frame)
  (setq appt-delete-window-function #'luk-appt-delete-frame))

(provide 'luk-appt)
