;;; -*- coding: utf-8; lexical-binding: t -*-

(provide 'luk-markdown)

(autoload 'markdown-mode ;; https://jblevins.org/projects/markdown-mode/
  "markdown-mode"
  "Major mode for editing Markdown files" t)

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
           ;; TODO: Fully trim any type of markdown heading too
           ((and (= b (line-beginning-position)) (region-modifiable-p b e)) ;; Only whitespace
            (delete-region b e))
           ((= len 2) ;; Hard linebreak
            (goto-char e))
           ((and (= len 1) (region-modifiable-p b e)) ;; Single whitespace
            (delete-region b e))
           ((and (> len 2) (region-modifiable-p (+ b 2) e)) ;; 2 + extra
            (delete-region (+ b 2) e))
           (t (goto-char e))))))))

(defun luk-markdown-hook-func ()
  ;; Markdown lines can get long (e.g. with tables), and having a
  ;; table row broken across multiple lines makes it unreadable.
  (setq truncate-lines t)

  ;; Use trailing whitespace deletion on save that respects markdown
  (setq luk-should-trim-whitespace nil)
  (add-hook 'before-save-hook 'luk-markdown-delete-trailing-whitespace nil 'make-local))

(defun luk-markdown-setup ()
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

  ;; Lines can get long (e.g. tables)
  (add-hook 'markdown-mode-hook #'luk-markdown-hook-func)

  (with-eval-after-load "markdown-mode"
    (define-key markdown-mode-map (kbd "C-c g") #'markdown-follow-thing-at-point)))
