;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'org-bullets) ; https://github.com/sabof/org-bullets
(require 'org)
(require 'org-attach)
(require 'org-id)
(require 'luk-hydra)
(require 'luk-util)
(require 'calendar)

(defgroup luk-org nil "Variables for luk-org")

(setq luk-org--clipboard-to-file-dir
      (concat (file-name-directory (or load-file-name buffer-file-name)) "clipboard-to-file"))

(defcustom luk-org-python-command
  nil
  "Python executable path (for org-mode python utility functions)"
  :type 'string
  :group 'luk-org)

(defcustom luk-org-image-editor
  nil
  "External image editor for image attachments in org-mode"
  :type 'string
  :group 'luk-org)

(when (require 'luk nil t)
  (luk-add-group 'luk-org))


;; Additional faces (e.g. for TODO-keywords)
(defface luk-org-todo-started '((t :inherit org-todo))
  "face for STARTED todo-state")

(defface luk-org-todo-canceled '((t :inherit org-done))
  "face for CANCELED todo-state")



;; Functions for switching between pretty display with hidden entities
;; and formatted links, or a more raw format.

(defun luk-org--descriptive-links (enable)
  "See `org-toggle-link-display'

This home-brewn variant takes an argument to better support
`luk-org-toggle-display'

Switches between pretty links (enable=true) and full
source (enable=false)."
  (if enable
      (progn
        (org-remove-from-invisibility-spec '(org-link))
        (setq org-descriptive-links t))
    (progn
      (add-to-invisibility-spec '(org-link))
      (setq org-descriptive-links nil))))

(defun luk-org--pretty-entities (enable)
  "See `org-toggle-pretty-entities'

This home-brewnn variant takes an argument to better support
`luk-org-toggle-display'

This includes e.g. subscript and superscript and some LaTeX names,
call `org-entities-help for the org documentation."
  (when (not (eq enable org-pretty-entities))
    (setq-local org-pretty-entities enable)
    (when (not org-pretty-entities)
      (save-restriction
        (widen)
        (decompose-region (point-min) (point-max))))))

(defun luk-org-toggle-display ()
  "Toggle display of emphasis markers, descriptive links etc."
  (interactive)
  (when (bound-and-true-p org-capture-mode)
    (user-error "Can't toggle in indirect capture buffer"))
  (let ((pretty-display? (not org-hide-emphasis-markers)))
    (if pretty-display? (message "Descriptive links, hide emphasis-markers")
      (message "Raw links, show emphasis-markers"))
    (setq org-hide-emphasis-markers pretty-display?)
    (luk-org--descriptive-links (not pretty-display?))
    (when (eq (not pretty-display?) prettify-symbols-mode)
      (call-interactively 'prettify-symbols-mode))
    (luk-org--pretty-entities pretty-display?)
    (if pretty-display? (org-display-inline-images) (org-remove-inline-images))
    (org-restart-font-lock)))


;; Misc utils

(defun luk-org-delete-trailing-whitespace ()
  "Delete trailing whitespace except at empty org-headings.

This avoids the annoyance of having yet-undescribed headings
turned into plain asterisks due to their trailing space being
removed."
  (interactive)
  (save-mark-and-excursion
    (org-save-outline-visibility
        (save-match-data
          (beginning-of-buffer)
          (while (re-search-forward "[ ]+$" nil t)
            (let* ((b (match-beginning 0)) (e (match-end 0)) (len (- e b)))
              (cond
               ((string-match
                 "^\\([*]+\\)\\([ ]+\\)$"
                 (buffer-substring
                  (line-beginning-position)
                  (line-end-position)))
                ;; At empty heading, delete any extra spaces, leaving one
                (if (region-modifiable-p (match-beginning 2) (match-end 2))
                    (delete-region
                     (+ (line-beginning-position) (match-beginning 2) 1)
                     (+ (line-beginning-position) (match-end 2)))
                  (goto e)))
               ((region-modifiable-p b e)
                (delete-region b e))
               (t (goto e)))))))))

(defun luk-org--filename-to-title (file-name)
  (luk-capitalize-first-word
   (replace-regexp-in-string "-" " " (file-name-base file-name))))

(defun luk-org-insert-boilerplate ()
  "Insert some typical start content for new org-files"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (insert "#+title: " (luk-org--filename-to-title (buffer-file-name)) "\n#+startup: indent overview\n\n* "))
  (goto-char 10))


;; Paste image functionality

(defun luk-org-run-clipboard-script (DIR)
  "Run “clipboard-to-file.py” and return the name of the image it
wrote to DIR.

DIR should exist prior to calling the function.

The returned name is the filename only, the folder path is
excluded."
  (with-temp-buffer
    (let ((RESULT (call-process
                   luk-org-python-command
                   nil
                   (current-buffer)
                   nil
                   (concat luk-org--clipboard-to-file-dir "/clipboard_to_file.py")
                   DIR)))
      ;; Signal errors, if any
      (cond
       ((= RESULT 0) ;; Success
        ;; Read filename from output
        (beginning-of-buffer)
        (buffer-substring (line-beginning-position) (line-end-position)))
       ((= RESULT 1) (error "No folder argument passed to clipboard_to_file.py"))
       ((= RESULT 2) (error "Folder \"%s\" does not exist" DIR))
       ((= RESULT 3) (user-error "No image in clipboard"))
       (t (error "Unknown error %d from clipboard_to_file.py" RESULT))))))

(defun luk-org-paste-image ()
  "Add an image from the clipboard as an org attachment and insert a link to it.

Prompts for a filename after displaying the image in the buffer.

Uses the Python script “clipboard_to_file.py” to retrieve the image
from the clipboard and the variable ‘luk-org-python-command’ to
find the Python interpreter for running the script."
  (interactive)
  (when (not luk-org-python-command)
    (user-error "luk-org-python-command not set"))
  (when (not (eq major-mode 'org-mode))
    (user-error "Not in org-mode"))

  ;; Create an org-attachment folder for the current node
  (let* ((DIR (org-attach-dir-get-create))
         ;; Start and end positions of the preview-attachment path in
         ;; the buffer
         (START nil)
         (END nil))
    (when (not DIR)
      (error "Failed to get/create org attachment dir"))

    ;; Get image from clipboard
    (let ((filename (luk-org-run-clipboard-script DIR)))

      ;; Set the :ATTACHMENT: tag to the node
      (org-attach-tag)

      ;; Insert a link to display the image with the temporary name
      ;; generated by clipboard_to_file.py
      (setq START (point))
      (insert (format "[[attachment:%s]]" filename))
      (setq END (point))

      (org-redisplay-inline-images)

      ;; Read a filename in minibuffer, rename the attached file from
      ;; the generated name and redisplay the image with the new name
      ;;
      ;; TODO: Displaying the image is so dramatic that it is easy to
      ;; not notice the minibuffer. Maybe need a better way to do this,
      ;; but I also want to show the image before requesting a name.
      (let* ((NEW-NAME (read-string "Name: " filename))
             (NEW-PATH (concat DIR "/" NEW-NAME)))
        (when (not (string= NEW-NAME filename))
          (rename-file (concat DIR "/" filename) NEW-PATH)
          (delete-region START END)
          (goto-char START)
          (insert (concat "[[attachment:" NEW-NAME "]]"))
          (org-redisplay-inline-images))))))

(defun luk-org--mode-hook ()
  ;; Use prettify-symbols to get "nicer" checkboxes
  (push '("[ ]" . "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  (push '("#+title: " . "") prettify-symbols-alist)
  (push '("#+begin_src" . "") prettify-symbols-alist)
  (push '("#+end_src" . "") prettify-symbols-alist)
  (push '("#+begin_quote" . "") prettify-symbols-alist)
  (push '("#+end_quote" . "") prettify-symbols-alist)
  (push '("#+startup:" . "") prettify-symbols-alist)
  (push '("#+RESULTS:" . "⟶") prettify-symbols-alist)
  (prettify-symbols-mode)

  ;; unicode bullets for org-titles instead of asterisks
  (org-bullets-mode 1)

  ;; Use readable links initially
  (luk-org--descriptive-links nil)

  ;; Use org-specific delete-trailing-whitespace function, which
  ;; preserves a single space after empty org headings.
  (setq luk-should-trim-whitespace nil)
  (add-hook 'before-save-hook 'luk-org-delete-trailing-whitespace nil 'make-local)

  (when (luk-new-file-buffer-p)
    (luk-org-insert-boilerplate)))


;; Functions for my org-specific hydra (see the hydra-package).
;;
;; Note: Extra blank lines in caption to use same height
;; as luk-hydra
(defhydra luk-org-hydra (:hint nil)
  (format "\
Main ➤ %s      _._: up
^─^──────────────────────────
_p_: toggle raw/pretty
_a_: archive subtree
_l_: org-lint
_P_: Paste image attachment


_q_: quit"
          (luk-caption "Org"))
  ("." (luk-hydra-push 'luk-org-hydra/body "org") :exit t)
  ("M-<up>" (luk-hydra-push 'luk-org-hydra/body "org") :exit t)
  ("p" luk-org-toggle-display)
  ("P" luk-org-paste-image :exit t)
  ("a" org-archive-subtree-default-with-confirmation)
  ("l" org-lint)
  ("q" nil :exit t))

(defun luk-org-summon-hydra ()
  (interactive)
  (luk-org-hydra/body))


;; Contextual hydra

(defvar luk-org--context-element nil
  "The latest context element for the `luk-org-context-hydra'.\n
Set to `org-element-context' on hydra start.")

(defun luk-org--context-key (key)
  "Extract a key from the `luk-org--context-element'."
  (plist-get (cadr luk-org--context-element) key))

(defun luk-org-delete-context-element ()
  "Delete the region described by `luk-org--context-element'"
  (let ((begin (luk-org--context-key :begin))
        (end (luk-org--context-key :end)))
    (kill-region begin end)))

(defun luk-org-link-element-edit ()
  "Edit the link currently described by `luk-org--context-element'"
  (let* ((current-link (luk-org--context-key :raw-link))
         (contents-begin (luk-org--context-key :contents-begin))
         (contents-end (luk-org--context-key :contents-end))
         (current-title
          (if contents-begin
              (buffer-substring-no-properties contents-begin
                                              contents-end)
            ""))
         (new-link (read-string "Link: " current-link))
         (new-title (read-string "Title: " current-title)))
    (delete-region (luk-org--context-key :begin)
                   (luk-org--context-key :end))
    (if (/= 0 (length new-title))
        (insert "[[" new-link "][" new-title "]]")
      (insert "[[" new-link "]]")))
  (setq luk-org--context-element nil))

(defun luk-org-link-element-copy-markdown ()
  "Copy the link described by `luk-org--context-element' to clipboard with markdown formatting."
  (let* ((link (luk-org--context-key :raw-link))
         (contents-begin (luk-org--context-key :contents-begin))
         (contents-end (luk-org--context-key :contents-end))
         (title
          (if contents-begin
              (buffer-substring-no-properties contents-begin
                                              contents-end)
            nil)))
    (kill-new
     (if title
         (concat "[" title "](" link ")")
       link))))

(defun luk-org-element-value-symbol (enum-name)
  "Get the enum-value for ENUM-NAME corresponding to the
org document content.

That is, split the `luk-org--context-element's \":value\" string on
spaces, and return the first enum-value symbol whose `symbol-name' is in
that list, or \\='- if none.

So when `luk-org--context-element' has been initialized from

    #+startup: indent overview

The invocation (luk-org-element-value-symbol 'Visibility) will
return 'overview"

  ;; todo: lower-case the list
  (let ((value (split-string (luk-org--context-key :value) " ")))
    (let ((symbol
           (cl-dolist (x (luk/enum-values enum-name))
             (when (member (symbol-name x) value)
               (cl-return x)))))
      (if (not symbol) '-
        symbol))))

(defun luk-org-open-in-image-editor ()
  (when (not luk-org-image-editor)
    (user-error "luk-org-image-editor not set."))
  (let ((path
         (if (string= (luk-org--context-key :type) "attachment")
             (concat (org-attach-dir) "/" (luk-org--context-key :path))
           (luk-org--context-key :path))))
    (start-process "image-editor" nil luk-org-image-editor path)))

(defun luk-org-set-image-width ()
  (let ((width (read-number "Width: ")))
    (goto-char (luk-org--context-key :begin))
    (let ((indent (- (point) (line-beginning-position))))


      (forward-line -1)
      (goto-char (line-beginning-position))
      (if (re-search-forward "^[ ]*\\(#\\+attr_org: :width.*\\)$" (line-end-position) t)
          (replace-match (format "#+ATTR_ORG: :width %d" width) nil t nil 1)
        (goto-char (luk-org--context-key :begin))
        (insert (format "#+ATTR_ORG: :width %d\n" width))
        (org-indent-line))
      (forward-line 1)
      (org-redisplay-inline-images))))

(defhydra luk-org-link-hydra (:hint nil)
  (format "\
Main ➤ %s      _._: up
^─^──────────────────────────
_e_: edit
_d_: delete
_m_: copy markdown
_q_: quit"
          (luk-caption "Org Link"))
  ("." (luk-hydra-push 'luk-org-link-hydra/body "org") :exit t)
  ("d" (luk-org-delete-context-element) :exit t)
  ("e" (luk-org-link-element-edit) :exit t)
  ("m" (luk-org-link-element-copy-markdown) :exit t)
  ("q" nil :exit t))

(defhydra luk-org-image-link-hydra (:hint nil)
  (format "\
Main ➤ %s      _._: up
^─^──────────────────────────
_e_: edit link
_E_: edit image in external editor
_w_: Set width
_d_: delete
_m_: copy markdown
_q_: quit"
          (luk-caption "Org Image"))
  ("." (luk-hydra-push 'luk-org-link-hydra/body "org") :exit t)
  ("d" (luk-org-delete-context-element) :exit t)
  ("e" (luk-org-link-element-edit) :exit t)
  ("w" (luk-org-set-image-width) :exit t)
  ("E" (luk-org-open-in-image-editor) :exit t)
  ("m" (luk-org-link-element-copy-markdown) :exit t)
  ("q" nil :exit t))

(defhydra luk-org-table-hydra (:hint nil)
  (format "\
Main ➤ %s      _._: up
^─^──────────────────────────
_a_: align       _←_: Move column left   _k r_: kill row       _i r_: insert row
_e_: edit field  _→_: Move column right  _k c_: delete column  _i c_: insert column
_c_: convert     _↑_: Move row up        ^   ^                 _i l_: insert line
^ ^              _↓_: Move row down      ^   ^
_q_: quit"
          (luk-caption "Table"))
  ("." (luk-hydra-push 'luk-org-link-hydra/body "org") :exit t)
  ("a" (org-table-align) :exit t)
  ("c" (org-table-convert) :exit t)
  ("e" (call-interactively #'org-table-edit-field) :exit t)
  ("<left>" (org-table-move-column-left) :exit nil)
  ("<right>" (org-table-move-column-right) :exit nil)
  ("<up>" (org-table-move-row-up) :exit nil)
  ("<down>" (org-table-move-row-down) :exit nil)
  ("k r" (org-table-kill-row) :exit nil)
  ("k c" (org-table-delete-column) :exit nil)
  ("i r" (let ((current-prefix-arg '(1))) (call-interactively #'org-table-insert-row)) :exit nil)
  ("i c" (let ((current-prefix-arg '(1))) (call-interactively #'org-table-insert-column)) :exit nil)
  ("i l" (org-table-insert-hline))
  ("q" nil :exit t))

(defvar luk-org--startup-old nil "The original value for the startup field.

This allows restoring on cancel.")

(defvar luk-org--indent nil "Enum for iterating over org indentation values.")
(luk/def-enum Indent ('- 'indent 'noindent))

(defvar luk-org--visibility nil "Enum for iterating over org visibility values.")
(luk/def-enum Visibility ('- 'overview 'content 'showall 'show2levels 'show3levels 'show4levels 'show5levels 'showeverything))

(defvar luk-org--numeration 'nil "Enum for iterating over org numeration values.")
(luk/def-enum Numeration ('- 'num 'nonum))

(defvar luk-org--timestamp-type nil "Timestamp type (TODO: worth using enum?)")
(defvar luk-org--timestamp-date nil "")
(defvar luk-org--temp-end nil "")

(defun luk-org--update-startup ()
  "Update the #+STARTUP element described by `luk-org--context-key.

Modify the text to correspond to the currently cycled to values in the hydra."
  (let ((old-point (point)))
    (goto-char (luk-org--context-key :begin))
    (delete-region (point) (line-end-position))
    (let ((new-content
           (concat
            "#+startup: "
            (string-join
             ;; Strip empty ("-") entries to avoid getting extra
             ;; spaces in string-join
             (seq-filter (lambda (v) (not (string= "-" v)))
                         (list (luk-org-enum-string luk-org--indent)
                               (luk-org-enum-string luk-org--visibility)
                               (luk-org-enum-string luk-org--numeration)))
             " ")
            )))
      (goto-char (luk-org--context-key :begin))
      (insert new-content))
    (if (< old-point (line-end-position))
        (goto-char old-point)
      (goto-char (line-end-position)))))

(defun luk-org--startup-cancel ()
  "Cancel edits to the startup-hydra"
  (save-excursion
    (delete-region (luk-org--context-key :begin)
                   luk-org--temp-end)

      (goto-char (luk-org--context-key :begin))
      (insert luk-org--startup-old)))

(defun luk-org--cycle-enum (instance-name instance)
  "Hydra-helper: Cycle the enum and update the buffer-text"
  (set instance-name (luk/enum-next instance))
  (luk-org--update-startup))

(defun luk-org--update-timestamp ()
  (let ((old-point (point)))
    (delete-region (luk-org--context-key :begin) luk-org--temp-end)
    (goto-char (luk-org--context-key :begin))
    (insert (if (eq luk-org--timestamp-type 'active) "<" "["))
    (insert (format "%d-%02d-%02d"
                    (plist-get luk-org--timestamp-date :year-start)
                    (plist-get luk-org--timestamp-date :month-start)
                    (plist-get luk-org--timestamp-date :day-start)))
    (insert (if (eq luk-org--timestamp-type 'active) ">" "]"))
    ;; Insert day (e.g. "Thu") if missing
    (org-timestamp-change 0 'day)
    (goto-char old-point)))

(defun luk-org--timestamp-select-calendar ()
  (let ((date (split-string (org-read-date) "-")))
    (plist-put luk-org--timestamp-date :year-start (string-to-number (car date)))
    (plist-put luk-org--timestamp-date :month-start (string-to-number (cadr date)))
    (plist-put luk-org--timestamp-date :day-start (string-to-number (caddr date))))
  (luk-org--update-timestamp))

(defun luk-org--timestamp-cycle-type ()
  (setq luk-org--timestamp-type
        (cl-case luk-org--timestamp-type
          ('active 'inactive)
          ('inactive 'active)))
  (luk-org--update-timestamp))

(defun luk-org--timestamp-set-today ()
  (let ((date (calendar-current-date)))
    (plist-put luk-org--timestamp-date :year-start (calendar-extract-year date))
    (plist-put luk-org--timestamp-date :month-start (calendar-extract-month date))
    (plist-put luk-org--timestamp-date :day-start (calendar-extract-day date)))
  (luk-org--update-timestamp))

(defun luk-org-enum-string (instance)
  "Return the enum value as a string"
  (symbol-name (luk/enum-value instance)))

(defun luk-org-image-link? ()
  (luk/image-filename-p (luk-org--context-key :path)))

(defhydra luk-org-startup-hydra (:hint nil :foreign-keys warn :post (org-ctrl-c-ctrl-c))
  (format "\
Main ➤ %s      _._: up
^─^──────────────────────────
_i_: Indentation: %%s(luk-org-enum-string luk-org--indent)
_v_: Visibility: %%s(luk-org-enum-string luk-org--visibility)
_n_: Numeration: %%s(luk-org-enum-string luk-org--numeration)
_R_: Restore startup visibility.

_o_: Ok  _c_: Cancel"
          (luk-caption "Org Keyword: STARTUP"))
  ("." (luk-hydra-push 'luk-org-startup-hydra/body "org-startup") :exit t)
  ("i" (luk-org--cycle-enum 'luk-org--indent luk-org--indent))
  ("v" (luk-org--cycle-enum 'luk-org--visibility luk-org--visibility))
  ("n" (luk-org--cycle-enum 'luk-org--numeration luk-org--numeration))
  ("R" (org-mode-restart))
  ("<return>" nil :exit t)
  ("o" nil :exit t)
  ("q" nil :exit t)
  ("<escape>" (luk-org--startup-cancel) :exit t)
  ("c" (luk-org--startup-cancel) :exit t))

(defhydra luk-org-timestamp-hydra (:hint nil :foreign-keys warn)
  (format "\
Main ➤ %s      _._: up
^─^──────────────────────────
_t_: Type: %% -28`luk-org--timestamp-type
_s_: Select in calendar
_n_: Set to today
_q_: Quit"
          (luk-caption "Org Keyword: Timestamp"))
  ("." (luk-hydra-push 'luk-org-timestamp-hydra/body "org") :exit t)
  ("t" (luk-org--timestamp-cycle-type))
  ("n" (luk-org--timestamp-set-today))
  ("s" (luk-org--timestamp-select-calendar) :exit t)
  ("RET" nil :exit t)
  ("q" nil :exit t))

(defun luk-org-context-hydra ()
  (interactive)
  (setq luk-org--context-element (org-element-context))
  (setq luk-org--temp-end (luk-org--context-key :end))
  (let ((el (car luk-org--context-element)))
    (cond ((eq el 'link)

           ;; Link
           (if (luk-org-image-link?) (luk-org-image-link-hydra/body)
             (luk-org-link-hydra/body)))

          ;; Timestamp
          ((and (eq el 'timestamp) (member (luk-org--context-key :type) '(inactive active))) ;; exclude ranges
           (setq luk-org--timestamp-type (luk-org--context-key :type))
           (setq luk-org--timestamp-date (list :year-start (luk-org--context-key :year-start)
                                               :month-start (luk-org--context-key :month-start)
                                               :day-start (luk-org--context-key :day-start)))
           (luk-org-timestamp-hydra/body))

          ;; #+startup-keyword
          ((and (eq el 'keyword) (string= (luk-org--context-key :key) "STARTUP"))
           (setq luk-org--startup-old (buffer-substring (luk-org--context-key :begin) (luk-org--context-key :end)))
           (setq luk-org--indent (luk/enum-instance 'Indent (luk-org-element-value-symbol 'Indent)))
           (setq luk-org--visibility (luk/enum-instance 'Visibility (luk-org-element-value-symbol 'Visibility)))
           (setq luk-org--numeration (luk/enum-instance 'Numeration (luk-org-element-value-symbol 'Numeration)))
           (luk-org-startup-hydra/body))

          ;; Table
          ((or (eq el 'table) (eq el 'table-cell) (eq el 'table-row))
           ;; Todo: Also match e.g. marked up text in a table via ":parent"
           (luk-org-table-hydra/body))

          ;; No match
          (t (message "Unknown element: %s" luk-org--context-element)))))

(defun luk-org-mode-setup ()
  ;;; Keys
  (define-key org-mode-map (kbd "C-c g") 'org-open-at-point)
  (define-key org-mode-map (kbd "C-c a") 'org-agenda)
  (define-key org-mode-map (kbd "<f6>") 'luk-org-toggle-display)
  (define-key org-mode-map (kbd "M-.") 'luk-org-summon-hydra)
  (define-key org-mode-map (kbd "C-c l") 'org-store-link)
  (define-key org-mode-map (kbd "<tab>") 'luk-tab-complete-smart-tab)
  (define-key org-mode-map (kbd "M-,") 'luk-org-context-hydra)

  ;; Bind org-store-link globally, to allow copying links in non-org
  ;; files and linking to them in an org-file.
  (global-set-key (kbd "C-c l") 'org-store-link)

  ;; Indent on newline
  (define-key org-mode-map (kbd "RET") 'org-return-and-maybe-indent)

  ;; Allow following links with enter, not only C-x o
  (setq org-return-follows-link  t)

  ;; Use "➤" after folded headings instead of "..."
  (setq org-ellipsis " ➤")

  ;; No underline for ➤
  ;; todo: Move into themes
  (set-face-attribute 'org-ellipsis nil :underline nil :foreground "#ffffff")

  (add-hook 'org-mode-hook 'luk-org--mode-hook)

  ;; Bigger check-boxes
  (set-face-attribute 'org-checkbox nil :height 1.5)

  ;; By default, hide emphasis markers like the = and * for
  ;; =verbatim= and *bold* (Toggle with f6)
  (setq org-hide-emphasis-markers t)

  ;; https://stackoverflow.com/questions/40332479/org-mode-folding-considers-whitespace-as-content
  (setq org-cycle-separator-lines -1)

  ;; Set ids for interactive link-store actions (e.g. C-c l), but
  ;; not when used in scripts, like for capture to avoid getting
  ;; unnecessary ids for nodes that I just happened to be at when
  ;; starting an org-capture.
  (setq org-id-link-to-org-use-id 'create-if-interactive)

  ;; Require braces after underscore for interpreting as subscript,
  ;; to avoid that "world" should be a subscript in "hello_world"
  ;; (e.g. with org-pretty-entities).
  ;;
  ;; The syntax to actually use subscript becomes instead:
  ;; "hello_{world}"
  (setq org-use-sub-superscripts 1)

  ;; Use yellow for STARTED task-states
  (setq org-todo-keyword-faces '(("STARTED" . luk-org-todo-started)
                                 ("CANCELED" . luk-org-todo-canceled)))

  (setq org-link-abbrev-alist '(("find-function" . "elisp:(find-function-other-window '%h)")
                                ("describe-function" . "elisp:(describe-function '%h)")
                                ("describe-variable" . "elisp:(describe-variable '%h)")
                                ("describe-package" . "elisp:(describe-package '%h)")
                                ("commit" . "elisp:(magit-show-commit \"%h\")")))

  ;; Don't try to put tags in a column, just space them one step
  ;; from the heading Mostly because I don't like that the ellipsis
  ;; symbol for collapsed headings goes after the tags, which gets
  ;; very far to the right for such headings.
  (setq org-tags-column 0)

  ;; Use image width attributes, if available, for inlined images
  (setq org-image-actual-width nil)

  ;; Open "file:"-links in dired instead of os-application
  (add-to-list 'org-file-apps '(directory . emacs))

  ;; Use shift+direction as windmove in org-mode when not at a special
  ;; location (e.g. heading or timestamp).
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)
  (setq org-startup-with-inline-images t))

(provide 'luk-org-mode)
