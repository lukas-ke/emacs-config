;;; -*- coding: utf-8; lexical-binding: t -*-

;; Top level hydra-menu for any mode
;;
;; That is, `luk-hydra/body' shows a menu with simple keys
;; for commands I commonly use.

(require 'hydra)
(require 'cl-lib)
(require 'luk-util)
(require 'luk-diff) ;; for luk-view-changes

(defgroup luk-hydra-faces nil "Faces for my hydra menus")

(when (require 'luk nil 'noerror)
  (luk-add-face-group 'luk-hydra-faces))

(defface luk-hydra-caption-face '((t :inherit default :weight bold))
  "Face that can be used for captions in hydras."
:group 'luk-hydra-faces)

;; For restoring
(defconst luk-hydra-amaranth-original-message hydra-amaranth-warn-message
  "For restoring the original amaranth message on hydra close")

(defun luk-hydra-checkbox (value)
  (if value "[x]" "[ ]"))

;;; Mode hydras:
;;; Allow calling the luk-hydra from a mode-specific hydra-menu
;;; and then return to the other menu with '.'
(setq luk-mode-hydra-name nil
      luk-mode-hydra-func nil)

(defun luk-hydra-push (f name)
  "Push a hydra body-function and a name to represent it,
to allow returning to it from the main hydra"
  (setq luk-mode-hydra-func f)
  (setq luk-mode-hydra-name name)
  (luk-hydra/body))

(defun luk-hydra-pop ()
  (interactive)
  "Push a hydra body-function and a name to represent it,
to allow returning to it from the main hydra"
  (when luk-mode-hydra-name
    (let ((PREV-HYDRA luk-mode-hydra-func))
      (setq luk-mode-hydra-name nil)
      (setq luk-mode-hydra-func nil)
      (funcall PREV-HYDRA))))

(defun luk-pop-hydra-str ()
  "Label to show for returning to the mode-hydra (or nothing if
luk-hydra was started directly"
  (if luk-mode-hydra-name
      (format "return to %s" luk-mode-hydra-name)
    "quit"))

(defun luk-rename-buffer-and-file ()
  "Rename buffer and file (if any) to name read from minibuffer."
  (interactive)
  (let ((OLD-FILE-NAME (buffer-file-name))
        (WAS-MODIFIED (buffer-modified-p)))
    (cond
     (OLD-FILE-NAME
      ;; Rename buffer and file if it exists
      (let* ((NEW-FILE-NAME (read-string "New file name: " OLD-FILE-NAME))
             (NEW-BUFFER-NAME (file-name-nondirectory NEW-FILE-NAME))
             (OLD-EXISTED (file-exists-p OLD-FILE-NAME)))
        (if OLD-EXISTED
            (progn
              ;; Rename file and buffer
              (rename-file OLD-FILE-NAME NEW-FILE-NAME)
              (set-visited-file-name NEW-FILE-NAME)
              (set-buffer-modified-p WAS-MODIFIED))
          ;; Just rename buffer, no file exists
          (set-visited-file-name NEW-FILE-NAME)
          (set-buffer-modified-p t)
          (message "Warning: Renamed buffer, but visited file does not exist - save buffer to create it."))))

     ((buffer-name)
      ;; Buffer not associated with a file, rename buffer only
      (let* ((NEW-BUFFER-NAME (read-string "New buffer name: " (buffer-name))))
        (rename-buffer NEW-BUFFER-NAME t))))))

(defun luk-delete-file ()
  "Delete the visited file, if any"
  (interactive)
  (let ((FILE-NAME (buffer-file-name)))
    (cond
     ((not FILE-NAME)
      (message "Buffer not associated with a file: \"%s\"." (buffer-name)))
     ((not (file-exists-p FILE-NAME))
      (message "Visited file does not exist:  \"%s\"" FILE-NAME))
     (t
      (when (yes-or-no-p (format "Delete \"%s\"?" FILE-NAME))
        (delete-file FILE-NAME 'trash-if-enabled)
        (set-buffer-modified-p t)
        (message "File deleted: \"%s\"." FILE-NAME))))))

(defun luk-copy-file-path ()
  "Add path to current file to kill-ring"
  (interactive)
  (let ((FILE-NAME (buffer-file-name)))
    (when FILE-NAME
      (kill-new FILE-NAME))))

(defun luk-caption (STR)
  (propertize STR 'face 'luk-hydra-caption-face))

(defun luk-find-in-files ()
  (interactive)
  (require 'ag)
  (call-interactively 'ag))

(if (require 'expand-region nil t)
    (defhydra luk-hydra-region (:hint nil :columns 1)
      ("c" er/contract-region "Contract")
      ("e" er/expand-region "Expand")
      ("r" replace-rectangle "Replace rectangle")
      ("q" nil "quit" :exit t))
  (defun luk-hydra-region/body ()
    (interactive)
    (message "Package %s not installed.\nInstall with \"M-x package-install expand-region\"."
             (propertize "expand-region" 'face 'bold))))

;; Display a big calendar with the calfw-package + org if available,
;; otherwise just use the built-in calendar.
(defalias 'luk-show-calendar
  (if (require 'calfw-org nil 'noerror)
      #'cfw:open-org-calendar
    #'calendar))


(defhydra luk-hydra (:hint nil
                           :foreign-keys warn
                           :pre (setq hydra-amaranth-warn-message "Invalid key.")
                           :post (setq hydra-amaranth-warn-message luk-hydra-amaranth-original-message))
  (format "\
%s^^^^^^^^^^^^     │ %s^^^^^             │ %s^^^^^^^^^      │ %s^^^^^^              │ %s^^^^              │ %s
^─^────────────────┼─^─^─────────────────┼─^───^────────────┼─^─^───────────────────┼─^───^───────────────┼─────────────────────
_t_: treemacs      │ _l_: find files     │ _b a_ set        │ _M_ menu bar toggle   │ _v c_ Calendar      │ _c t_ Todo
_e_: explorer here │ _f_: .. in files    │ _b l_ list       │ _m_ menu bar open     │ _v a_ Week agenda   │ _c b_ Bookmark
_R_: rename        │ _C-x C-r_ recent    │ _b j_ jump       │ _S_ scroll bar toggle │ _v t_ Todo list     │ _c m_ Memo
_D_: delete        │ _C-x f_ in git repo │ _b J_ jump other │ _T_ select theme..    │ _v A_ Agenda choice │ _c w_ Waiting
_C_: copy path     │ ^ ^                 │ ^   ^            │ _o_ Mode line..       │ _g_ magit status    │ _c l_ Goto last
_d_: view changes  │ ^ ^                 │ ^   ^            │ _a_ Appointments..    │ _r_ region menu     │ _c c_ Pick template
_q_: %s"
          (luk-caption "Current File")
          (luk-caption "Files")
          (luk-caption "Bookmarks")
          (luk-caption "Window")
          (luk-caption "View")
          (luk-caption "Capture")
          "%s(luk-pop-hydra-str)")

  ("." luk-hydra-pop :exit t)
  ;; Current
  ("t" treemacs :exit t)
  ("e" luk-explore :exit t)
  ("R" luk-rename-buffer-and-file :exit t)
  ("D" luk-delete-file :exit t)
  ("C" luk-copy-file-path :exit t)
  ("d" luk-view-changes :exit t)

  ;; Files
  ("l" luk-list-files :exit t)
  ("f" luk-find-in-files :exit t)

  ;; Bookmarks
  ("b a" bookmark-set :exit t)
  ("b l" list-bookmarks :exit t)
  ("b j" bookmark-jump :exit t)
  ("b J" bookmark-jump-other-window :exit t)

  ;; Window
  ("M" menu-bar-mode :exit nil)
  ("m" menu-bar-open :exit t)
  ("S" scroll-bar-mode :exit nil)
  ("T" luk-hydra-theme/body :exit t)
  ("o" luk-mode-line-hydra/body :exit t)
  ("a" luk-appt-hydra/body :exit t)

  ;; View
  ("g" luk-magit-status :exit t)
  ("r" luk-hydra-region/body :exit t)
  ("v c" (luk-show-calendar) :exit t)
  ("v a" (org-agenda-list 'a) :exit t)
  ("v t" (org-todo-list 'a) :exit t)
  ("v A" (org-agenda 'a) :exit t)

  ;; Capture
  ("c c" (org-capture) :exit t)
  ("c t" (org-capture nil "t") :exit t)
  ("c b" (org-capture nil "b") :exit t)
  ("c m" (org-capture nil "m") :exit t)
  ("c w" (org-capture nil "w") :exit t)
  ("c l" org-capture-goto-last-stored :exit t)
  ("C-x C-r" #'luk-recentf-open :exit t)
  ("C-x f" #'find-file-in-git-repo :exit t)
  ("q" luk-hydra-pop :exit t))

(defun luk-hydra-summon ()
  (interactive)
  ;; Clear old pushed hydras, e.g. if the hydra was closed with C-g
  (luk-hydra-pop)
  (luk-hydra/body))

(defvar luk-navigate-start-buffer nil "Initial buffer when luk-hydra-window was opened")

(defun luk-switch-to-start ()
  (interactive)
  (message "Switching to %s" luk-navigate-start-buffer))

(defhydra luk-hydra-window (:hint nil :exit nil
                                  :foreign-keys warn
                                  :pre (setq hydra-amaranth-warn-message "Invalid key.")
                                  :post (setq hydra-amaranth-warn-message luk-hydra-amaranth-original-message))
  "
Window select: arrows                            ^^^^^^│ In Window, Page _8_: Backward _9_: Forward    _q_ to quit
       delete: _0_/_d_: current,_1_: others            │            Buffer  _a_:previous _b_: next
       split:  _2_:  below, _3_: right (or S-arrow)  ^^│"
  ("<up>" windmove-up)
  ("<right>" windmove-right)
  ("<down>" windmove-down)
  ("<left>" windmove-left)
  ("0" delete-window)
  ("d" delete-window)
  ("1" delete-other-windows)
  ("2" split-window-below)
  ("3" split-window-right)
  ("<S-down>" (progn (split-window-below) (windmove-down))) ("S" (progn (split-window-below) (windmove-down)))
  ("<S-right>" (progn (split-window-right) (windmove-right))) ("D" (progn (split-window-right) (windmove-right)))
  ("b" switch-to-buffer)
  ("f" find-file)
  ("k" kill-current-buffer)
  ("B" balance-windows)
  ("o" other-window)
  ("O" (other-window -1))
  ("i" (other-window -1))
  ("]" (forward-page))
  ("9" (forward-page))
  ("[" (backward-page))
  ("8" (backward-page))
  ("v" (switch-to-buffer luk-navigate-start-buffer))
  ("a" (previous-buffer))
  ("s" (next-buffer))
  ("<return>" nil :exit t)
  ("q" nil :exit t))

(defun luk-show-window-hydra ()
  (interactive)
  (setq luk-navigate-start-buffer (current-buffer))
  (luk-hydra-window/body))

(provide 'luk-hydra)
