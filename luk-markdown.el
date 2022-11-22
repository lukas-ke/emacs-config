;;; -*- coding: utf-8; lexical-binding: t -*-

(provide 'luk-markdown)
(require 'luk-hydra)
(require 'luk-util)
(require 'cus-edit) ; For customize-set-variable
(require 'org) ;; For org-ellipsis face

(autoload 'markdown-mode ;; https://jblevins.org/projects/markdown-mode/
  "markdown-mode"
  "Major mode for editing Markdown files" t)

;; Modified from markdown-mode to recognize headers with trailing
;; whitespace as headers for trimming-purposes.
(defconst luk-markdown-sloppy-regex-header
  "^\\(?:\\(?1:[^\r\n\t -].*\\)\n\\(?:\\(?2:=+\\)\\|\\(?3:-+\\)\\)\\|\\(?4:#+[ \t]+\\)\\(?5:.*?\\)\\(?6:[ \t]*#*\\)\\)[ ]*$")

(defun luk-markdown-header-level ()
  "Returns the level of the current markdown header"
  (interactive)
  (save-match-data
    (if (not (thing-at-point-looking-at markdown-regex-header))
        nil
      (cond
       ((match-string 2)
        ;; This style is level 1
        ;; =====================
        1)
       ((match-string 3)
        ;; This style is level 2
        ;; ---------------------
        2)
       ((match-string 4)
        ;; ## The level for this style is the #-count
        (length (string-remove-suffix " " (match-string 4))))
       (t
        ;; Unknown header style
        nil)))))

(defun luk-markdown-delete-trailing-whitespace ()
  "Delete trailing respecting markdown hard line breaks.

Markdown uses two spaces for a hard linebreak so this command:
- deletes any single trailing space at line-ends,
- leaves instances of exactly two trailing spaces unmodified,
- trims more than two spaces to exactly two,
- trims whitespace completely at lines with only spaces, regardless of count."
  (interactive)
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[ ]+$" nil t)
        (let* ((b (match-beginning 0)) (e (match-end 0)) (len (- e b)))
          (cond
           ((and (= b (line-beginning-position)) (region-modifiable-p b e))
            ;; Only whitespace
            (delete-region b e))

           ((and (region-modifiable-p b e) (goto-char (line-beginning-position)) (thing-at-point-looking-at luk-markdown-sloppy-regex-header))
            ;; Whitespace on header line, delete all trailing spaces,
            ;; even double-space as a markdown hard line break makes
            ;; no sense there.
            (delete-region b e)
            (forward-line 1))

           ((= len 2)
            ;; Markdown double-space is a hard linebreak, leave it
            ;; intact.
            (goto-char e))

           ((and (= len 1) (region-modifiable-p b e))
            ;; A single whitespace, remove it.
            (delete-region b e))

           ((and (> len 2) (region-modifiable-p (+ b 2) e))
            ;; More than two spaces, delete down to two
            ;; TODO: Maybe it would be good to warn for this case, and
            ;; list the locations, but how would I get that message to
            ;; appear after the save message?
            (delete-region (+ b 2) e))

           (t (goto-char e))))))))

(defun luk-markdown-tab-maybe-cycle ()
  "Specialization of `luk-tab-complete-custom' for markdown.

Forward to markdown-cycle when appropriate, otherwise allow
luk-tab-complete to expand text.

This function returns t when it took care of the tab, nil
otherwise which means luk-tab-complete should do its thing."
  (interactive)
  (save-mark-and-excursion
    (save-match-data
      (cond
       (mark-active nil)
       ((looking-at "\\_>") nil)
       ((thing-at-point-looking-at markdown-regex-header)
        (message "At header")
        (markdown-cycle) t)
       (t t)))))

(defun luk-markdown-hook-func ()
  ;; Markdown lines can get long (e.g. with tables), and having a
  ;; table row broken across multiple lines makes it unreadable.
  (setq truncate-lines t)

  (setq luk-tab-complete-custom #'luk-markdown-tab-maybe-cycle)

  ;; Use trailing whitespace deletion on save that respects markdown
  (setq luk-should-trim-whitespace nil)
  (add-hook 'before-save-hook 'luk-markdown-delete-trailing-whitespace nil 'make-local)

  (setq luk-mode-hydra #'luk-markdown-mode-hydra/body)

  ;; Use "➤" instead of "..." as indication for collapsed text. I can't
  ;; say I quite understand this, it is based on:
  ;; https://emacs.stackexchange.com/a/17815
  (let ((display-table (if buffer-display-table
                           buffer-display-table
                         (make-display-table))))
    (unless buffer-display-table
      (setq buffer-display-table display-table))
    (set-display-table-slot display-table 4
                            (vconcat (mapcar (lambda (c)
                                               (make-glyph-code c 'org-ellipsis)) "➤")))))

(defun luk-md-meta-left ()
  (interactive)
  (save-match-data
    (if (thing-at-point-looking-at markdown-regex-header)
        (when (> (luk-markdown-header-level) 1)
          (markdown-promote))
      (left-word))))

(defun luk-md-meta-right ()
  (interactive)
  (save-match-data
    (if (thing-at-point-looking-at markdown-regex-header)
        (when (< (luk-markdown-header-level) 5)
          (markdown-demote))
      (right-word))))

(defhydra luk-markdown-mode-hydra (:hint nil
                                      :foreign-keys warn
                                      :exit nil
                                      :pre (setq hydra-amaranth-warn-message "Invalid key (Markdown hydra)")
                                      :post (setq hydra-amaranth-warn-message luk-hydra-amaranth-original-message))
  "
_m_ Visible markup %s(luk-hydra-checkbox (not markdown-hide-markup))
_i_ Inline images  %s(luk-hydra-checkbox markdown-inline-image-overlays)
_t_ Insert table
_M_ Popup menu
_q_ Quit"
  ("m" #'markdown-toggle-markup-hiding)
  ("i" #'markdown-toggle-inline-images)
  ("t" #'markdown-insert-table :exit t)
  ("M" (luk/popup-menu markdown-mode-menu) :exit t)
  ("q" nil :exit t))

(defun luk-markdown-setup ()
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (setq markdown-asymmetric-header t) ;; Do not insert a closing # for atx-style headers
  (setq markdown-fontify-code-blocks-natively t)
  ;; Lines can get long (e.g. tables)
  (add-hook 'markdown-mode-hook #'luk-markdown-hook-func)

  (with-eval-after-load 'markdown-mode

    (customize-set-variable
     ;; Using customize-set-variable to run the :set-function that
     ;; updates header faces.
     'markdown-header-scaling t)
    (define-key markdown-mode-map (kbd "C-c g") #'markdown-follow-thing-at-point)
    (define-key markdown-mode-map (kbd "M-<left>") #'luk-md-meta-left)
    (define-key markdown-mode-map (kbd "C-<left>") #'luk-md-meta-left)
    (define-key markdown-mode-map (kbd "M-<right>") #'luk-md-meta-right)
    (define-key markdown-mode-map (kbd "C-<right>") #'luk-md-meta-right)
    )))
