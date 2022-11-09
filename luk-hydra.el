;;; -*- coding: utf-8; lexical-binding: t -*-

;; Top level hydra-menu for any mode
;;
;; That is, `luk-hydra/body' shows a menu with simple keys
;; for commands I commonly use.

(require 'hydra)
(require 'cl-lib)
(require 'luk-util)
(require 'luk-diff) ;; for luk-view-changes
(require 'luk-calfw)
(require 'perfect-margin)
(require 'hungry-delete)
(require 'rainbow-mode)

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
  "Delete the visited file respecting `delete-by-moving-to-trash'.

If `delete-by-moving-to-trash' is true, the buffer is also
killed, so to rename a file, use `luk-rename-buffer-and-file' instead."
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
        (if delete-by-moving-to-trash
            (progn
              (when (not (kill-buffer (current-buffer)))
                (set-buffer-modified-p t))
              (message "File put in the trash."))
          (set-buffer-modified-p t)
          (message "File deleted.")))))))

(defun luk-copy-file-path ()
  "Add path to current file to kill-ring (or marked files in `dired-mode').

This function adds the full path to the file visited in the
current buffer to the `kill-ring'.

When in dired-mode, instead copy the path to the marked-files (or
file at point), separated by endlines. When no files are marked
and point is not at a file, fall-back to the
`default-directory' (the current dired-directory)."
  (interactive)
  (let ((file-name (or (buffer-file-name)
                       (when (derived-mode-p 'dired-mode)
                           (let ((files (dired-get-marked-files)))
                             (if files
                                 (string-join files "\n")
                               default-directory))))))
    (if file-name
        (kill-new file-name)
      (user-error "Buffer not associated with a file — no path copied"))))

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
  (if (require 'calfw-org nil 'noerror) #'luk-cfw-show-calendar
    #'calendar))

(defmacro luk/def-context-hydra (name pretty-name &optional docstring &rest heads)
  "Streamline creation of my Context-hydras"
  (declare (indent defun) (doc-string 2))
  `(defhydra ,name
     (:hint nil
            :pre (setq hydra-amaranth-warn-message ,(format "Invalid key (%s menu)" pretty-name))
            :post (setq hydra-amaranth-warn-message luk-hydra-amaranth-original-message)
            :exit nil
            :foreign-keys warn)
     ,docstring
     ,@heads))

(defvar-local luk-context-hydra nil "Function to call from `luk-show-context-hydra'")

(defun luk-show-context-hydra ()
  (interactive)
  (if (fboundp luk-context-hydra)
      (funcall luk-context-hydra)
    (message "No context hydra available")))

(defvar-local luk-mode-hydra nil "Function to call from `luk-show-mode-hydra'")

(defun luk-show-mode-hydra ()
  (interactive)
  (if (fboundp luk-mode-hydra)
      (funcall luk-mode-hydra)
    (message "No mode hydra available")))

(defhydra luk-hydra (:hint nil
                           :foreign-keys warn
                           :pre (setq hydra-amaranth-warn-message "Invalid key (Main hydra)")
                           :post (setq hydra-amaranth-warn-message luk-hydra-amaranth-original-message))
  (format "\
%s^^^^^^^^^^^^    │ %s^^^^^             │ %s^^^^^^^^^      │ %s^^^^             │ %s^^^^              │ %s
^─^───────────────┼─^─^─────────────────┼─^───^────────────┼─^─^────────────────┼─^───^───────────────┼─────────────────────
_t_ treemacs      │ _l_ find files      │ _b a_ set        │ _g_ magit status   │ _v c_ Calendar      │ _c t_ Todo
_e_ explorer here │ _f_ .. in files     │ _b l_ list       │ _G_ magit commands │ _v a_ Week agenda   │ _c b_ Bookmark
_R_ rename        │ _C-x C-r_ recent    │ _b j_ jump       │ _r_ region menu…   │ _v t_ Todo list     │ _c m_ Memo
_D_ delete        │ _C-x f_ in git repo │ _b J_ jump other │ _m_ menu bar open… │ _v A_ Agenda choice │ _c w_ Waiting
_C_ copy path     │ ^ ^                 │ ^   ^            │ ^ ^                │ _a_   Appointments… │ _c l_ Goto last
_d_ view changes  │ ^ ^                 │ ^   ^            │ ^ ^                │ ^ ^                 │ _c c_ Pick template
_q_ quit          │ ^ ^                 │ ^   ^            │ ^ ^                │ ^ ^                 │"
          (luk-caption "Current File")
          (luk-caption "Files")
          (luk-caption "Bookmarks")
          (luk-caption "More")
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
  ("m" menu-bar-open :exit t)
  ("a" luk-appt-hydra/body :exit t)

  ;; View
  ("g" luk-magit-status :exit t)
  ("G" #'magit-file-dispatch :exit t)
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
  ("C-x f" #'luk-find-file-in-git-repo :exit t)
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
                                  :pre (setq hydra-amaranth-warn-message "Invalid key (Window hydra)")
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

(defun luk-scroll-bar-mode ()
  "scroll-bar-mode is nil, 'right or 'left.

I don't care about 'left, so turn it into t or nil for the
luk-settings-hydra's formatting"
  (not (eq scroll-bar-mode nil)))

(defhydra luk-settings-hydra (:hint nil
                                    :exit nil
                                    :foreign-keys warn
                                    :pre (setq hydra-amaranth-warn-message "Invalid key (Settings hydra)")
                                    :post (setq hydra-amaranth-warn-message luk-hydra-amaranth-original-message))
  "
%s(luk-caption \"Editing\")                        %s(luk-caption \"UI\")                %s(luk-caption \"More\")
_e q_ Electric quote       %-3s`electric-quote-mode^   _s_ Scrollbars %-3s(luk-scroll-bar-mode)  _t_ Theme…
_e i_ Electric indent      %-3s`electric-indent-mode   _M_ Menu bar   %-3s`menu-bar-mode^^  _o_ Mode line…
_e p_ Electric pair        %-3s`electric-pair-mode^^   ^ ^            ^^^^^^^^^^^^^^^^^     _R_ Rainbow mode %-3s`rainbow-mode
_h_   Hungry delete        %-3s`hungry-delete-mode^^
_f_   Fido                 %-3s`fido-mode^^^^^^^^^^^
_m_   Perfect margin       %-3s`perfect-margin-mode^
_r_   Recursive minibuffer %-3s`enable-recursive-minibuffers
"
  ("e q" #'electric-quote-mode)
  ("e i" #'electric-indent-mode)
  ("e p" #'electric-pair-mode)
  ("h" #'hungry-delete-mode)
  ("f" #'fido-mode)
  ("m" #'perfect-margin-mode)
  ("r" (setq enable-recursive-minibuffers (not enable-recursive-minibuffers)))
  ("s" #'scroll-bar-mode)
  ("M" #'menu-bar-mode)
  ("t" #'luk-hydra-theme/body :exit t)
  ("o" #'luk-mode-line-hydra/body :exit t)
  ("R" #'rainbow-mode)
  ("q" nil :exit t))

(defun luk-show-window-hydra ()
  (interactive)
  (setq luk-navigate-start-buffer (current-buffer))
  (luk-hydra-window/body))

(provide 'luk-hydra)
