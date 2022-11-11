;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'org-superstar)
(require 'org)
(require 'org-attach)
(require 'org-id)
(require 'luk-hydra)
(require 'luk-util)
(require 'luk-calfw)
(require 'calendar)
(require 'appt)
(setq luk-org-calfw-available
      (if (require 'calfw-org nil 'noerror)
          t
        nil))

(defgroup luk-org nil "Variables for luk-org.")

(defgroup luk-org-faces nil "Faces for luk-org."
  :group 'luk-org)

(when (require 'luk nil 'noerror)
  (luk-add-group 'luk-org)
  (luk-add-face-group 'luk-org-faces))

(defvar luk-org--clipboard-to-file
  (concat (file-name-directory (or load-file-name buffer-file-name))
          "clipboard-to-file/clipboard_to_file.py")
  "Path to the script used for writing images from clipboard as files for org-attachments.")

(defvar luk-org--file-to-clipboard
  (concat (file-name-directory (or load-file-name buffer-file-name))
          "clipboard-to-file/file_to_clipboard.py")
  "Path to the script used for copying org-attachment images to the clipboard.")


(defcustom luk-org-python-command
  nil
  "Python executable path (for `org-mode' python utility functions)."
  :type 'string
  :group 'luk-org)


;; Additional faces (e.g. for TODO-keywords)
(defface luk-org-todo-started '((t :inherit org-todo))
  "Face for STARTED todo-state."
  :group 'luk-org-faces)

(defface luk-org-todo-canceled '((t :inherit org-done))
  "Face for CANCELED todo-state."
  :group 'luk-org-faces)


;; Functions for switching between pretty display with hidden entities
;; and formatted links, or a more raw format.

(defun luk-org--descriptive-links (enable)
  "See `org-toggle-link-display'.

This home-brewn variant takes an argument to better support
`luk-org-toggle-display'

Switches between pretty links (ENABLE=t) and full
source (ENABLE=nil)."
  (if enable
      (progn
        (org-remove-from-invisibility-spec '(org-link))
        (setq org-descriptive-links t))
    (progn
      (add-to-invisibility-spec '(org-link))
      (setq org-descriptive-links nil))))

(defun luk-org--pretty-entities (enable)
  "See `org-toggle-pretty-entities'.

This home-brewn variant takes an argument, ENABLE, to better
support `luk-org-toggle-display'

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

(defun luk-org-agenda-goto-cfw-calendar ()
  "Open the calendar from the calfw package with an org-agenda-date."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (let* ((day (or (get-text-property (min (1- (point-max)) (point)) 'day)
		  (user-error "Don't know which date to open in calendar")))
	 (date (calendar-gregorian-from-absolute day)))
    (cfw:open-org-calendar)
    (with-current-buffer "*cfw-calendar*"
      (cfw:navi-goto-date date))))

(defun luk-org-refresh-appointments ()
  "Reinitialize `appt-time-msg-list' from agenda"
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt)
  (appt-activate 1))

(defun luk-org-delete-trailing-whitespace ()
  "Delete trailing whitespace except at empty org-headings.

This avoids the annoyance of having yet-undescribed headings
turned into plain asterisks due to their trailing space being
removed."
  (interactive)
  (save-mark-and-excursion
    (org-save-outline-visibility
        (save-match-data
          (goto-char (point-min))
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
  "Return a title based on FILE-NAME."
  (luk-capitalize-first-word
   (replace-regexp-in-string "-" " " (file-name-base file-name))))

(defun luk-org-insert-boilerplate ()
  "Insert some typical start content for new org-files."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "#+title: " (luk-org--filename-to-title (buffer-file-name)) "\n#+startup: indent overview\n\n* "))
  (goto-char 10))


;; Paste image functionality

(defun luk-org-run-clipboard-script (DIR)
  "Run “clipboard-to-file.py” and return a file name.

Returns the name of the image that was written to DIR.

DIR should exist prior to calling the function.

The returned name is the filename only, the folder path is
excluded."
  (with-temp-buffer
    (let ((RESULT (call-process
                   luk-org-python-command
                   nil
                   (current-buffer)
                   nil
                   luk-org--clipboard-to-file
                   DIR)))
      ;; Signal errors, if any
      (cond
       ((= RESULT 0) ;; Success
        ;; Read filename from output
        (goto-char (point-min))
        (buffer-substring (line-beginning-position) (line-end-position)))
       ((= RESULT 1) (error "No folder argument passed to clipboard_to_file.py"))
       ((= RESULT 2) (error "Folder \"%s\" does not exist" DIR))
       ((= RESULT 3) (user-error "No image in clipboard"))
       (t (error "Unknown error %d from clipboard_to_file.py" RESULT))))))

(defun luk-org-copy-image-to-clipboard ()
  (interactive)
  (let ((luk-org--context-element (org-element-context)))
    (when (not (and (eq (car luk-org--context-element) 'link) (luk-org-image-link?)))
      (user-error "Point not in an image link"))
    (let* ((link-target (luk-org--context-key :raw-link))
           (file-name
            (if (and (string-prefix-p "attachment:" link-target) (org-attach-dir))
                (concat (org-attach-dir)
                        "/"
                        (string-remove-prefix "attachment:" link-target))
              (user-error "Point not in an image attachment"))))
      (with-temp-buffer
        (let ((RESULT (call-process
                       luk-org-python-command
                       nil
                       (current-buffer)
                       nil
                       luk-org--file-to-clipboard
                       file-name)))
          ;; Signal errors, if any
          (cond
           ((= RESULT 0) ;; Success
            (message "Image copied to clipboard."))
           ((= RESULT 10) (error "No file argument passed to file_to_clipboard.py"))
           ((= RESULT 20) (error "File not found (%s)" file-name))
           ((= RESULT 30) (user-error "PIL error"))
           (t (error "Unknown error %d from clipboard_to_file.py" RESULT))))))))

(defun luk-org-paste-image ()
  "Add an image from the clipboard as an org attachment and insert a link to it.

Prompts for a filename after displaying the image in the buffer.

Uses the Python script “clipboard_to_file.py” to retrieve the image
from the clipboard and the variable ‘luk-org-python-command’ to
find the Python interpreter for running the script."
  (interactive)
  (when (not luk-org-python-command)
    (user-error "Error: luk-org-python-command not set!"))
  (when (not (derived-mode-p 'org-mode))
    (user-error "Error: This function can only be used in org-mode!"))

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

      ;; Set the :ATTACH: tag to the node
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

(defun luk-org-tab-maybe-cycle ()
  (interactive)
  (if mark-active
      nil
    (if (looking-at "\\_>")
        (if (= ?* (char-before))
            ;; Don't expand after * in org, it gets weird. Instead let
            ;; org-cycle do its thing.
            ;;
            ;; TODO: Not perfect, if dabbrev expands "word" to "word*",
            ;; tab will instead start cycling which is surprising.
            (progn (org-cycle) t)
          nil)
      ;; Not at end of word in org-mode -> org cycle
      ;; Warning: Can recurse infinitely if `luk-tab-complete-smart-tab'
      ;; is bound to TAB instead of <tab>.
      (org-cycle) t)))


(defun luk-org--mode-hook ()
  "Hook for `org-mode'."

  ;; Use prettify-symbols to get "nicer" checkboxes
  ;; (See also luk-font.el, for mapping the code-points to fonts)
  (push '("[ ]" . "") prettify-symbols-alist)
  (push '("[X]" . "" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  (push '("#+title: " . "") prettify-symbols-alist)
  (push '("#+begin_src" . "") prettify-symbols-alist)
  (push '("#+end_src" . "") prettify-symbols-alist)
  (push '("#+begin_quote" . "") prettify-symbols-alist)
  (push '("#+end_quote" . "") prettify-symbols-alist)
  (push '("#+startup:" . "") prettify-symbols-alist)
  (push '("#+todo:" . "📝") prettify-symbols-alist)
  (push '("#+RESULTS:" . "⟶") prettify-symbols-alist)

  (prettify-symbols-mode)

  (setq luk-tab-complete-custom #'luk-org-tab-maybe-cycle)

  ;; unicode bullets for org-titles instead of asterisks
  (org-superstar-mode)

  ;; Use readable links initially
  (luk-org--descriptive-links nil)

  ;; Use org-specific delete-trailing-whitespace function, which
  ;; preserves a single space after empty org headings.
  (setq luk-should-trim-whitespace nil)
  (add-hook 'before-save-hook 'luk-org-delete-trailing-whitespace nil 'make-local)

  (setq luk-mode-hydra #'luk-org-hydra/body)
  (setq luk-context-hydra #'luk-org-context-hydra)

  (when (luk-new-file-buffer-p)
    (luk-org-insert-boilerplate)))

(defun luk-org--capture-hook ()
  "Hook for configuring `org-capture' windows."

  ;; Try to make it harder to lose a capture buffer.
  ;; (Probably not bullet-proof, but worth a try).
  (let ((w (get-buffer-window (current-buffer))))
    ;; Prevent deleting a capture window on C-x 1 in other windows
    (set-window-parameter w 'no-delete-other-windows t)
    (set-window-dedicated-p w t))

  ;; Collapse drawers in the capture-buffer. Somehow this
  ;; doesn't affect the target buffer.
  (org-hide-drawer-all))

;; Functions for my org-specific hydra (see the hydra-package).
;;
;; Note: The text should have the same height as luk-hydra to make
;; switching between them look nicer.

(defun luk-org--raw-pretty-str ()
  "Return fontified raw/pretty for luk-org-hydra."
  (concat
   (if org-hide-emphasis-markers "Raw" (propertize "Raw" 'face 'luk-hydra-caption-face))
   " | "
   (if org-hide-emphasis-markers (propertize "Pretty" 'face 'luk-hydra-caption-face) "Pretty")))

(defhydra luk-org-hydra (:hint nil
                               :foreign-keys warn
                               :pre (setq hydra-amaranth-warn-message "Invalid key (org hydra)")
                               :post (setq hydra-amaranth-warn-message luk-hydra-amaranth-original-message))
  (format "\
Main ➤ %s      _._: up
^─^──────────────────────────
_p_: Display: %%s(luk-org--raw-pretty-str)
_a_: Archive subtree
_r_: Refile
_l_: Lint
_P_: Paste image attachment

_q_: Quit"
          (luk-caption "Org"))
  ("." (luk-hydra-push 'luk-org-hydra/body "org") :exit t)
  ("M-<up>" (luk-hydra-push 'luk-org-hydra/body "org") :exit t)
  ("p" luk-org-toggle-display)
  ("P" luk-org-paste-image :exit t)
  ("a" org-archive-subtree-default-with-confirmation :exit t)
  ("r" org-refile :exit t)
  ("l" org-lint :exit t)
  ("q" nil :exit t))


;; Contextual hydra

(defvar luk-org--context-element nil
  "The latest context element for the `luk-org-context-hydra'.

Set to `org-element-context' on hydra start.")

(defun luk-org-set-context-element ()
  (setq luk-org--context-element (org-element-context))
  (setq luk-org--temp-end (luk-org--context-key :end)))

(defun luk-org--context-key (key)
  "Extract value of KEY from `luk-org--context-element'."
  (plist-get (cadr luk-org--context-element) key))

(defun luk-org-delete-context-element ()
  "Delete the region described by `luk-org--context-element'."
  (let ((begin (luk-org--context-key :begin))
        (end (luk-org--context-key :end)))
    (kill-region begin end)))

(defun luk-org-find-attributes-before (pos)
  (save-excursion
    (goto-char pos)
    (forward-line -1)
    (while (string-match-p "[[:blank:]]*#\\+attr_" (buffer-substring (line-beginning-position) (line-end-position)))
      (forward-line -1))
    (forward-line 1)
    (line-beginning-position)))

(defun luk-org-delete-image ()
  "Delete the image link at point, its related attributes and maybe its file.

If it is an attachment, ask if it should be deleted. If the
attachment folder is empty after deleting an image, delete the
folder."
  (interactive)
  (luk-org-set-context-element)
  (when (not (and (eq (car luk-org--context-element) 'link) (luk-org-image-link?)))
    (user-error "Point not in an image link."))
  (let* ((begin (luk-org--context-key :begin))
         (end (luk-org--context-key :end))
         (attribute-start (luk-org-find-attributes-before begin))
         (link-target (luk-org--context-key :raw-link)))
    ;; Delete the image-link and any attributes
    (delete-region attribute-start end)

    ;; If the image is an attachment, ask if the file should be deleted
    (if (and (string-prefix-p "attachment:" link-target) (org-attach-dir))
        (let ((filename
               (concat (org-attach-dir)
                       "/"
                       (string-remove-prefix "attachment:" link-target))))
          (when (and
                 (file-exists-p filename)
                 (yes-or-no-p "Delete file from disk? "))
            ;; Use `delete-file' instead of `org-attach-delete-one' to respect
            ;; `delete-by-moving-to-trash'
            (delete-file filename t)

            ;; Delete the attachment directory if this was the last
            ;; file
            (if (and (directory-empty-p (org-attach-dir)))
                ;; This will output "Attachment directory deleted.",
                ;; therefore the "Image deleted."-message is only in
                ;; the ELSE.
                (org-attach-delete-all t)
              (message "Image deleted.")))))))

(defun luk-org-inside-drawer (context-element)
  "Return the drawer start pos if inside a drawer, else nil."
  (interactive)
  (let ((parent (plist-get (cadr context-element) :parent)))
    (if (not parent)
        nil
      (if (eq (car parent) 'drawer)
          (plist-get (cadr parent) :begin)
        (luk-org-inside-drawer parent)))))

(defun luk-org-wrap-image-in-drawer ()
  "Wrap the image at point (and its attributes) in an org-drawer."
  (interactive)
  (let ((luk-org--context-element (org-element-context)))
    (when (not (and (eq (car luk-org--context-element) 'link) (luk-org-image-link?)))
      (user-error "Point not in an image link."))
    (let ((current-drawer-pos (luk-org-inside-drawer luk-org--context-element)))
      (when current-drawer-pos
        (goto-char current-drawer-pos)
        (user-error "Image is already inside a drawer.")))
    (let* ((begin (luk-org--context-key :begin))
           (end (luk-org--context-key :end))
           (attribute-start (luk-org-find-attributes-before begin)))
      (save-mark-and-excursion
        (set-mark end)
        (goto-char attribute-start)
        (insert ":image:\n")
        (goto-char (mark))
        (insert "\n:end:")
        (org-redisplay-inline-images)))))

(defun luk-org-link-element-edit ()
  "Edit the link currently described by `luk-org--context-element'."
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
  "Get the enum-value for ENUM-NAME from `luk-org--context-key'.

That is, split the `luk-org--context-element's \":value\" string on
spaces, and return the first enum-value symbol whose `symbol-name' is in
that list, or \\='- if none.

So when `luk-org--context-element' has been initialized from

    #+startup: indent overview

The invocation (luk-org-element-value-symbol 'Visibility) will
return 'overview."

  ;; todo: lower-case the list
  (let ((value (split-string (luk-org--context-key :value) " ")))
    (let ((symbol
           (cl-dolist (x (luk/enum-values enum-name))
             (when (member (symbol-name x) value)
               (cl-return x)))))
      (if (not symbol) '-
        symbol))))

(defun luk-org-open-in-image-editor ()
  "Open linked image at point in external editor."
  (when (or (not luk-image-editor) (= 1 (length luk-image-editor)))
    (user-error "Error: luk-image-editor not set!"))
  (let ((path
         (if (string= (luk-org--context-key :type) "attachment")
             (concat (org-attach-dir) "/" (luk-org--context-key :path))
           (luk-org--context-key :path))))
    (start-process "image-editor" nil luk-image-editor path)))

(defun luk-org-set-image-width ()
  "Insert a width-attribute for an image."
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

(luk/def-context-hydra luk-org-link-hydra "Org Link"
"
Main ➤ %s(luk-caption \"Org Link\")
^─^──────────────────────────
_e_ edit
_d_ delete
_m_ copy markdown
_q_ quit"
  ("d" (luk-org-delete-context-element) :exit t)
  ("e" (luk-org-link-element-edit) :exit t)
  ("m" (luk-org-link-element-copy-markdown) :exit t)
  ("q" nil :exit t))


;; luk-org-image-link-hydra and friends

(defvar luk--pre-image-window-configuration nil
  "Window configuration before viewing image in full")

(define-derived-mode luk-org-image-mode image-mode
  "luk-org-image-mode"
  "Major mode for viewing images from org in full screen")

(defun luk--org-image-kill-restore ()
  (interactive)
  (kill-buffer)
  (set-window-configuration luk--pre-image-window-configuration))

(define-key luk-org-image-mode-map (kbd "q") #'luk--org-image-kill-restore)

(defun luk-org-view-image ()
  "View an image in a full window, go back with q"
  (interactive)
  (let ((luk-org--context-element (org-element-context)))
    (when (not (and (eq (car luk-org--context-element) 'link) (luk-org-image-link?)))
      (user-error "Point not in an image link"))
    (let* ((link-target (luk-org--context-key :raw-link))
           (file-name
            (if (and (string-prefix-p "attachment:" link-target) (org-attach-dir))
                (concat (org-attach-dir)
                        "/"
                        (string-remove-prefix "attachment:" link-target))
              link-target)))
      (setq luk--pre-image-window-configuration (current-window-configuration))
      (find-file file-name)
      (luk-org-image-mode)
      (delete-other-windows)
      (message "Use q to go back"))))

(luk/def-context-hydra luk-org-image-link-hydra "Org Image"
  "
Main ➤ %s(luk-caption \"Org Image\")
^─^──────────────────────────
_e_ edit link
_E_ open in external editor
_w_ set width
_W_ wrap in drawer
_v_ view
_d_ delete
_m_ copy markdown
_c_ copy to clipboard
_q_ quit"
  ("d" (luk-org-delete-image) :exit t)
  ("e" (luk-org-link-element-edit) :exit t)
  ("w" (luk-org-set-image-width) :exit t)
  ("W" (luk-org-wrap-image-in-drawer) :exit t) ;; TODO: Not if :parent is drawer
  ("v" (luk-org-view-image) :exit t)
  ("E" (luk-org-open-in-image-editor) :exit t)
  ("m" (luk-org-link-element-copy-markdown) :exit t) ;; TODO: Only if non-local
  ("c" (luk-org-copy-image-to-clipboard) :exit t)
  ("q" nil :exit t))

(luk/def-context-hydra luk-org-table-hydra "Org Table"
"
Main ➤ %s(luk-caption \"Org Table\")
^─^──────────────────────────
_a_: align       _←_: Move column left   _k r_: kill row       _i r_: insert row
_e_: edit field  _→_: Move column right  _k c_: delete column  _i c_: insert column
_c_: convert     _↑_: Move row up        ^   ^                 _i l_: insert line
^ ^              _↓_: Move row down      ^   ^
_q_: quit"
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

;; TODO: These docs and probably variables are a bit dumb
(defvar luk-org--timestamp-type nil "Timestamp type (TODO: worth using enum?).")
(defvar luk-org--timestamp-date nil "Property list for date.")
(defvar luk-org--temp-end nil "End position for deletion.")

(defun luk-org--startup-str ()
  "Return the string for the #+startup-keyword based on enums."

  (concat
   "#+startup: "
   (string-join
    ;; Strip empty ("-") entries to avoid getting extra
    ;; spaces in string-join
    (seq-filter (lambda (v) (not (string= "-" v)))
                (list (luk-org-enum-string luk-org--indent)
                      (luk-org-enum-string luk-org--visibility)
                      (luk-org-enum-string luk-org--numeration)))
    " ")))

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
  "Reverts edits done in the startup-hydra."
  (save-excursion
    (delete-region (luk-org--context-key :begin)
                   luk-org--temp-end)

      (goto-char (luk-org--context-key :begin))
      (insert luk-org--startup-old)))

(defun luk-org--startup-ok ()
  "Perform org-refresh when “#+startup”-modified."
  (when (not (string= luk-org--startup-old
                      (concat (luk-org--startup-str) "\n"))))
    (org-ctrl-c-ctrl-c))

(defun luk-org--cycle-enum (instance-name instance)
  "Hydra-helper: Cycle the enum then update the buffer-text.

INSTANCE-NAME is the same as INSTANCE, but quoted."
  ;; TODO: It ought to be possible to do this without passing the
  ;; symbol both quoted and unquoted
  (set instance-name (luk/enum-next instance))
  (luk-org--update-startup))

(defun luk-org--update-timestamp ()
  "Update the timestamp in the buffer from `luk-org--timestamp-date'."
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
  "Select a date in calendar using org-read-date.

Used for modifying a timestamp via the hydra."
  (let ((date (split-string (org-read-date) "-")))
    (plist-put luk-org--timestamp-date :year-start (string-to-number (car date)))
    (plist-put luk-org--timestamp-date :month-start (string-to-number (cadr date)))
    (plist-put luk-org--timestamp-date :day-start (string-to-number (caddr date))))
  (luk-org--update-timestamp))

(defun luk-org--timestamp-cycle-type ()
  "Cycle timestamp at point between active and inactive."
  (setq luk-org--timestamp-type
        (cl-case luk-org--timestamp-type
          ('active 'inactive)
          ('inactive 'active)))
  (luk-org--update-timestamp))

(defun luk-org--timestamp-set-today ()
  "Set timestamp at point to today's date."
  (let ((date (calendar-current-date)))
    (plist-put luk-org--timestamp-date :year-start (calendar-extract-year date))
    (plist-put luk-org--timestamp-date :month-start (calendar-extract-month date))
    (plist-put luk-org--timestamp-date :day-start (calendar-extract-day date)))
  (luk-org--update-timestamp))

(defun luk-org-enum-string (instance)
  "Return the enum value as a string.

This function is used for showing enum symbols as string values
in hydra documentation.

INSTANCE should be an unquoted enum-instance."
  (symbol-name (luk/enum-value instance)))

(defun luk-org-image-link? ()
  "True if `luk-org--context-key' appears to be an image link."
  (luk/image-filename-p (luk-org--context-key :path)))

(luk/def-context-hydra luk-org-startup-hydra "Org Startup"
"
Main ➤ %s(luk-caption \"Org Keyword: STARTUP\")
^─^──────────────────────────
_i_ Indentation: %s(luk-org-enum-string luk-org--indent)
_v_ Visibility: %s(luk-org-enum-string luk-org--visibility)
_n_ Numeration: %s(luk-org-enum-string luk-org--numeration)
_R_ Restore startup visibility.

_o_ Ok  _c_ Cancel"
  ("i" (luk-org--cycle-enum 'luk-org--indent luk-org--indent))
  ("v" (luk-org--cycle-enum 'luk-org--visibility luk-org--visibility))
  ("n" (luk-org--cycle-enum 'luk-org--numeration luk-org--numeration))
  ("R" (org-mode-restart))
  ("<return>" nil :exit t)
  ("o" (luk-org--startup-ok) :exit t)
  ("q" nil :exit t)
  ("<escape>" (luk-org--startup-cancel) :exit t)
  ("c" (luk-org--startup-cancel) :exit t))

(luk/def-context-hydra luk-org-timestamp-hydra "Org Timestamp"
  "
Main ➤ %s(luk-caption \"Org Keyword: Timestamp\")
^─^──────────────────────────
_t_ Type: %-28`luk-org--timestamp-type
_s_ Select in calendar
_n_ Set to today
_q_ Quit"
  ("t" (luk-org--timestamp-cycle-type))
  ("n" (luk-org--timestamp-set-today))
  ("s" (luk-org--timestamp-select-calendar) :exit t)
  ("RET" nil :exit t)
  ("q" nil :exit t))

(defun luk-org-context-hydra ()
  "Show a context-specific hydra.

Which of the hydras gets shown depends on the context around
point, retrieved using `org-element-context' which gets stored in
`luk-org--context-element' so that hydra functions can use the
information."
  (interactive)
  (luk-org-set-context-element)
  (let ((el (car luk-org--context-element)))
    (cond ((eq el 'link)

           ;; Link
           (if (luk-org-image-link?)
               (luk-org-image-link-hydra/body)
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


(defun luk-org--say-abort-instead ()
  "\\<org-capture-mode-map>Suggest to user to use \\[org-capture-kill] instead."
  (interactive)
  (message (substitute-command-keys "Abort instead with \\<org-capture-mode-map>\\[org-capture-kill].")))

(defun luk-org--say-commit-instead ()
  "\\<org-capture-mode-map>Suggest to user to use \\[org-capture-finalize] instead."
  (interactive)
  (message (substitute-command-keys "Finalize capture instead with \\<org-capture-mode-map>\\[org-capture-finalize].")))

(defun luk-org--capture-return ()
  "In capture buffers, move past property drawers on return at heading"
  (interactive)
  (cond ((not (org-at-heading-p))
         (org-return-and-maybe-indent))

        ((and org-return-follows-link
              (eq (car (org-element-context)) 'link)

              ;; Hacky: org-element-context gives link even after the link
              ;; when at end of line or after moving out of it from the left,
              ;; but org-open-at-point fails, so if at a space
              ;; allow enter to go to next line, or if at the end of the line
              (not (string= (buffer-substring-no-properties (point) (+ 1 (point))) " "))
              (not (= (point) (line-end-position))))
         (org-open-at-point))
        ((= (forward-line) 1)
         ;; Could not move forward, so at the end. Insert new line
         (org-return-and-maybe-indent))
        ((org-at-drawer-p)
         (progn
           (org-forward-element)
           (when (org-at-drawer-p)
             (forward-line)
             (when (org-at-drawer-p)
               (org-return-and-maybe-indent)))))))

(defun luk-org--capture-block-rank-change ()
  (interactive)
  ;; TODO: Improve this to allow moving captured subheadings
  (message "Probably not a good idea"))

(defun luk-org--capture-note-template ()
  (concat "* %U %?\n"
          ":PROPERTIES:\n"
          ;; TODO: Make these actual properties? Or other drawer name?
          ;; Or just make them part of a readable header?
          ":filed: %U\n"
          ":at:     " (format "%%F::%d\n" (line-number-at-pos (point)))
          ;; TODO: Add repo info
          ":END:\n"

          ;; Add region content if active
          (if (region-active-p)
              (progn
                (concat
                 ;; TODO: More robust to do a search link?
                 ;; Maybe add temporary bookmarks too
                 (format "\n[[%%F::%d][%%f]]" (line-number-at-pos (point)))
                 (format "\n#+begin_src %s\n" (string-remove-suffix "-mode" (symbol-name major-mode)))
                 ;; TODO: Extend multiline region to beginning of line at both beginning, end
                 ;; (For correct indentation)
                 (luk/dedent-string (buffer-substring (region-beginning) (region-end)))
                 "\n#+end_src"))
            (format "\n[[%%F::%d][%%f]]\n" (line-number-at-pos (point))))))

(defvar luk-org-insert-cycle 0)
(defun luk-org-insert-cycle-heading ()
  "Insert a heading or cycle its rank on repeat."
  (interactive)
  (if (eq last-command #'luk-org-insert-cycle-heading)
      ;; A heading was inserted or cycled by the previous command.
      ;; Cycle the heading.
      (cond
       ((= luk-org-insert-cycle 0)
        (org-do-demote)
        (setq luk-org-insert-cycle 1))
       ((= luk-org-insert-cycle 1)
        (org-do-promote)
        (org-do-promote)
        (setq luk-org-insert-cycle 2))
       ((= luk-org-insert-cycle 2)
        (org-do-demote)
        (setq luk-org-insert-cycle 0)))

    ;; The last command was not insertion or cycling, so insert a heading
    (save-excursion
      (setq luk-org-insert-cycle 0)
      (beginning-of-line)
      (org-insert-heading))

    (when (not (org-at-heading-p))
      ;; In some cases, org-insert-heading inserts it on the next line
      (forward-line 1))

    (when (= (char-after) ?*)
      ;; On blank lines, the beginning-of-line + save-excursion places
      ;; marker in the asterisks. That's inconvenient so move to the
      ;; end of line (reasonable insertion point for heading content)
      (end-of-line))))

(defun luk-org-mode-setup ()
  "Setup `org-mode' keys, hooks et. al."
  ;;; Keys

  (define-key org-mode-map (kbd "C-c g") 'org-open-at-point)
  (define-key org-mode-map (kbd "C-c a") 'org-agenda)
  (define-key org-mode-map (kbd "<f6>") 'luk-org-toggle-display)
  (define-key org-mode-map (kbd "C-c l") 'org-store-link)
  (define-key org-mode-map (kbd "<tab>") 'luk-tab-complete-smart-tab)

  ;; Unset cycle agenda files (I use this key for mode hydra menus instead)
  (define-key org-mode-map (kbd "C-,") nil)

  (when luk-org-calfw-available
    (define-key org-agenda-mode-map (kbd "c") #'luk-org-agenda-goto-cfw-calendar))

  ;; Prevent killing a capture buffer with C-x k (possibly leaving a
  ;; half-written-capture in the target file)
  (define-key org-capture-mode-map (kbd "C-x k") #'luk-org--say-abort-instead)

  (define-key org-capture-mode-map (kbd "C-x C-s") #'luk-org--say-commit-instead)

  (define-key org-capture-mode-map (kbd "<return>") #'luk-org--capture-return)

  (define-key org-capture-mode-map (kbd "M-<left>") #'luk-org--capture-block-rank-change)
  (define-key org-capture-mode-map (kbd "M-<right>") #'luk-org--capture-block-rank-change)

  ;; Prevent killing the window holding the capture buffer
  ;; causing me to forget to complete the capture.
  ;;
  ;; TODO: Maybe I should shrink it instead though, there could be a reason
  ;; to hide it.
  (define-key org-capture-mode-map (kbd "C-x 0") #'luk-org--say-abort-instead)

  ;; Bind org-store-link globally, to allow copying links in non-org
  ;; files and linking to them in an org-file.
  (global-set-key (kbd "C-c l") 'org-store-link)

  ;; Indent on newline
  (define-key org-mode-map (kbd "RET") 'org-return-and-maybe-indent)

  (define-key org-mode-map (kbd "§") #'luk-org-insert-cycle-heading)

  ;; Allow following links with enter, not only C-x o
  (setq org-return-follows-link  t)

  ;; Use "➤" after folded headings instead of "..."
  (setq org-ellipsis " ➤")

  (add-hook 'org-mode-hook 'luk-org--mode-hook)

  (add-hook 'org-capture-mode-hook 'luk-org--capture-hook)

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

  ;; Try to react to edits that affect hidden sections. Doesn't seem to
  ;; work for a lot of cases though, only when the heading itself is edited?
  ;;
  ;; I.e. the invisible region is only from heading start to expand symbol
  ;; It does not catch for example if I remove the bullet from a collapsed heading
  ;; and the text below is lost.
  (setq org-catch-invisible-edits 'show-and-error)

  ;; No validation link at the end of html generated with
  ;; org-html-export-to-html
  (setq org-html-validation-link nil)

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
