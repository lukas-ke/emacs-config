;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'org-bullets) ; https://github.com/sabof/org-bullets
(require 'org)
(require 'org-attach)
(require 'org-id)
(require 'luk-hydra)
(provide 'luk-org-mode)

(defgroup luk-org nil "Variables for luk-org")

(setq luk-org--clipboard-to-file-dir
      (concat (file-name-directory (or load-file-name buffer-file-name)) "clipboard-to-file"))

(defcustom luk-org-python-command
  nil
  "Python executable path (for org-mode python utility functions)"
  :type 'string
  :group 'luk-org)

(when (require 'luk nil t)
  (luk-add-group 'luk-org))

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

This home-brewn variant takes an argument to better support
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

(defun luk-org-run-clipboard-script (DIR)
  ;; Run clipboard_to_file.py for `luk-org-paste-image'
  (let ((RESULT (call-process
                 luk-org-python-command
                 nil
                 (get-buffer-create "*clipboard-to-file*")
                 nil
                 (concat luk-org--clipboard-to-file-dir "/clipboard_to_file.py")
                 DIR)))
    ;; Signal errors, if any
    (cond
     ((= RESULT 0) t) ;; Success
     ((= RESULT 1) (error "No folder argument passed to clipboard_to_file.py"))
     ((= RESULT 2) (error "Folder \"%s\" does not exist" DIR))
     ((= RESULT 3) (user-error "No image in clipboard"))
     (t (error "Unknown error %d from clipboard_to_file.py" RESULT)))))

(defun luk-org-paste-image ()
  "Add an image from the clipboard as an org attachment and insert a link to it.

Prompts for a filename after displaying the image in the buffer.

Uses the Python script “clipboard_to_file.py” to retrieve the image
from the clipboard and the variable `luk-org-python-command' to
find the Python interpreter for running the script.

TODO: I would like this to work with regular `yank' in org-mode,
      but that seems risky. Maybe by using
      `interprogram-paste-function' I could get that to work.
      Perhaps then generating a file-name would be best, and
      offering a practical function to rename a linked image?"
  (interactive)
  (when (not luk-org-python-command)
    (user-error "luk-org-python-command not set"))
  (when (not (eq major-mode 'org-mode))
    (user-error "Not in org-mode"))

  ;; Create an org-attachment folder for the current node
  (let* ((DIR (org-attach-dir-get-create))
         (FILE-PATH (concat DIR "/paste.png"))
         ;; Start and end of initial link path
         (START nil)
         (END nil))
    (when (not DIR)
      (error "Failed to get/create org attachment dir"))

    ;; Get image from clipboard
    (luk-org-run-clipboard-script DIR)

    ;; Set the :ATTACHMENT: tag to the node
    (org-attach-tag)

    ;; Insert a link to display the image with using the temporary
    ;; name "paste.png"
    (setq START (point))
    (insert (concat "[[attachment:paste.png]]"))
    (setq END (point))

    (org-redisplay-inline-images)

    ;; Read a filename in minibuffer, rename the attached file from
    ;; paste.png, and redisplay the image with the new name
    ;;
    ;; TODO: Displaying the image is so dramatic that it is easy to
    ;; not notice the minibuffer. Maybe need a better way to do this,
    ;; but I also want to show the image before requesting a name.
    (let* ((NEW-NAME (read-string "Name: " "paste.png"))
           (NEW-PATH (concat DIR "/" NEW-NAME)))
      (when (not (string= NEW-NAME "paste.png"))
        (rename-file FILE-PATH NEW-PATH)
        (delete-region START END)
        (goto-char START)
        (insert (concat "[[attachment:" NEW-NAME "]]"))
        (org-redisplay-inline-images)))))

(defun luk-org--mode-hook ()
  ;; Use prettify-symbols to get "nicer" checkboxes
  (push '("[ ]" . "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  (prettify-symbols-mode)

  ;; unicode bullets for org-titles instead of asterisks
  (org-bullets-mode 1)

  ;; Use readable links initially
  (luk-org--descriptive-links nil))

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

(defun luk-org-mode-setup ()
  (with-eval-after-load 'org

    ;;; Keys
    (define-key org-mode-map (kbd "C-c g") 'org-open-at-point)
    (define-key org-mode-map (kbd "C-c a") 'org-agenda)
    (define-key org-mode-map (kbd "<f6>") 'luk-org-toggle-display)
    (define-key org-mode-map (kbd "M-.") 'luk-org-summon-hydra)
    (define-key org-mode-map (kbd "C-c l") 'org-store-link)

    (define-key org-mode-map (kbd "<tab>") 'luk-tab-complete-smart-tab)

    ;; Indent on newline
    (define-key org-mode-map (kbd "RET") 'org-return-indent)

    ;; Use "➤" after folded headings instead of "..."
    (setq org-ellipsis " ➤")

    ;; No underline for ➤
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
    ;; to avoid that "world" shoudl be a subscript in "hello_world"
    ;; (e.g. with org-pretty-entities).
    ;;
    ;; The syntax to actually use subscript becomes instead:
    ;; "hello_{world}"
    (setq org-use-sub-superscripts 1)

    ;; Use yellow for STARTED task-states
    (setq org-todo-keyword-faces '(("STARTED" . "#fff200")))

    (setq org-link-abbrev-alist '(("find-function" . "elisp:(find-function-other-window '%h)")
                                  ("describe-function" . "elisp:(describe-function '%h)")
                                  ("describe-variable" . "elisp:(describe-variable '%h)")))

    ;; Don't try to put tags in a column, just space them one step
    ;; from the heading Mostly because I don't like that the ellipsis
    ;; symbol for collapsed headings goes after the tags, which gets
    ;; very far to the right for such headings.
    (setq org-tags-column 0)

    ;; Use image width attributes, if available, for inlined images
    (setq org-image-actual-width nil)

    ;; Open "file:"-links in dired instead of os-application
    (add-to-list 'org-file-apps '(directory . emacs))))
