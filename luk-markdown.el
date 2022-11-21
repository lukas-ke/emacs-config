;;; -*- coding: utf-8; lexical-binding: t -*-

(provide 'luk-markdown)

(autoload 'markdown-mode ;; https://jblevins.org/projects/markdown-mode/
  "markdown-mode"
  "Major mode for editing Markdown files" t)

;; Modified from markdown-mode to recognize headers with trailing
;; whitespace as headers for trimming-purposes.
(defconst luk-markdown-sloppy-regex-header
  "^\\(?:\\(?1:[^\r\n\t -].*\\)\n\\(?:\\(?2:=+\\)\\|\\(?3:-+\\)\\)\\|\\(?4:#+[ \t]+\\)\\(?5:.*?\\)\\(?6:[ \t]*#*\\)\\)[ ]*$")

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
  (add-hook 'before-save-hook 'luk-markdown-delete-trailing-whitespace nil 'make-local))

(defun luk-markdown-setup ()
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

  ;; Lines can get long (e.g. tables)
  (add-hook 'markdown-mode-hook #'luk-markdown-hook-func)

  (with-eval-after-load "markdown-mode"
    (define-key markdown-mode-map (kbd "C-c g") #'markdown-follow-thing-at-point)))
