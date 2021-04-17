;;; -*- coding: utf-8; lexical-binding: t -*-
;; The function `luk-list-files' and the related mode

(provide 'luk-list-files)

(defun luk--link-to-file-action (button)
  (let ((target-file (button-get button 'target-file))
        (link-buffer (current-buffer)))
    (save-selected-window
      (find-file-other-window (button-get button 'target-file)))))

(define-button-type 'luk-link-to-file-button
  'action 'luk--link-to-file-action
  'follow-link t
  'help-echo "Open file"
  'help-args "tests")

(defun luk--lf-insert-button (PATH)
  (let ((BUTTON (insert-button PATH :type 'luk-link-to-file-button)))
    (button-put BUTTON 'target-file PATH))
  (insert "\n"))

(defun luk--lf-ignore-git (PATH)
  "Predicate to ignore .git-folders for `directory-files-recursively'"
  (not (string-suffix-p "/.git" PATH)))

(defun luk-list-files (DIR PATTERN &optional SAME-WINDOW)
  "Show a list of files matching a regexp-pattern, recursively"
  ;; TODO: Consider not having a pattern here, and allowing filtering
  ;; later, or both.

  ;; E.g. apply filtering with f-key, hide non matching with overlays
  ;; or just rerender buffer and filter the list, save the list of
  ;; files
  (interactive "DRoot: \nsRE: ")
  (message "Searching..")
  (let ((FILES (directory-files-recursively
                DIR
                PATTERN
                nil
                'luk--lf-ignore-git)))
    (if (not FILES) (message "No files found in \"%s\" matching \"%s\"" DIR PATTERN)
      (message "Creating result")
      (let ((target-buffer (get-buffer-create "*luk-list-files*")))
        (with-current-buffer target-buffer

          (setq buffer-read-only nil)
          (erase-buffer)

          (insert "Recursive search results. Use ? for help\n")
          (insert "Root: " DIR "\n")
          (when (not (= (length PATTERN) 0))
            (insert "RE:  " PATTERN "\n"))
          (insert "\n")

          (mapcar 'luk--lf-insert-button FILES)
          (if SAME-WINDOW
              (switch-to-buffer target-buffer nil t)
            (switch-to-buffer-other-window target-buffer))
          (luk-list-files-mode)
          (setq-local LFP PATTERN LFD DIR)
          (setq buffer-read-only t)
          (message nil))))))

;; Mode setup
;;
;; Use a mode for the match window, to allow having a key map
;; TODO: Maybe I could use the mode to do links, instead of buttons
;;       to allow e.g. C-enter, enter
(define-derived-mode luk-list-files-mode fundamental-mode
  "luk-list-files"
  "Major mode for `luk-list-files' search results")


(defun luk--list-files-current-file-path ()
  (let ((button (car (overlays-at (point)))))
    (if button
        (button-get button 'target-file)
      nil)))

(defun luk--list-files-open-file-other-window ()
  "Open the file at the current line in other window and focus it.

Regular enter just opens the file, but keeps focus in the file
list, to allow skimming multiple files."
  (interactive)
  (let ((file-name (luk--list-files-current-file-path)))
    (when file-name
      (find-file-other-window file-name))))


(defun luk-list-file-names-src-target ()
    (let ((SRC nil) (DST nil))
      (setq SRC (luk--list-files-current-file-path))
      (when (not SRC) (error "No SRC file at point"))
      (setq DST (read-string "New name" SRC))
      (list SRC DST)))


(defun luk-list-file-names-update-button (NEW-PATH)
  (let ((button (car (overlays-at (point)))))
    (when button
      (button-put button 'target-file NEW-PATH)
      (button-put button 'display NEW-PATH))))

(defun luk-list-files-rename (SRC DST)
  "Rename the file at the current line"
  (interactive (luk-list-file-names-src-target))
  (rename-file SRC DST)
  (luk-list-file-names-update-button DST))

(defun luk-list-files-repeat-search ()
  "Run the search again (perhaps after renaming some files)"
  (interactive)
  (let ((POS (point)))
    (luk-list-files LFD LFP t)
    (goto-char POS)))

(defun luk-list-files-help ()
  (interactive)
  (describe-mode))

(defun luk-list-files-quit ()
  (interactive)
  (kill-current-buffer))


(define-key luk-list-files-mode-map (kbd "n") #'forward-line)
(define-key luk-list-files-mode-map (kbd "?") #'luk--list-files-help)
(define-key luk-list-files-mode-map (kbd "r") #'luk-list-files-rename)
(define-key luk-list-files-mode-map (kbd "g") #'luk-list-files-repeat-search)
(define-key luk-list-files-mode-map (kbd "q") #'luk-list-files-quit)
(define-key luk-list-files-mode-map (kbd "<C-return>") #'luk--list-files-open-file-other-window)
