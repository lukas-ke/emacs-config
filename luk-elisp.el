;;; -*- coding: utf-8; lexical-binding: t -*-

(provide 'luk-elisp)
(require 'luk-util)
(require 'luk-tab-complete)

(defun luk-elisp-insert-boilerplate ()
  "Insert magic comment for utf-8-coding and lexical binding"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (insert ";;; -*- coding: utf-8; lexical-binding: t -*-\n\n")))

(defun luk-elisp--hook()
  "Run luk-elisp-insert-boilerplate for new Emacs-lisp files."

  (when (require 'company nil 'noerror)
    (company-mode))

  (if (require 'form-feed nil 'noerror)
      (form-feed-mode)
    (push '("" . "▬" ) prettify-symbols-alist)
    (prettify-symbols-mode))

  (when (luk-new-file-buffer-p)
    (luk-elisp-insert-boilerplate)
    (forward-line)
    (forward-line)))

(defun luk-elisp-smart-tab ()
  "Complete or indent at point in emacs-lisp-mode.

Like ‘luk-tab-complete-smart-tab’ this either indents or
completes. It uses company-mode for completion if enabled
and not inside a comment.

Another change from ‘luk-tab-complete-smart-tab’ is that this
function only supports emacs-lisp, while it does not try to
handle minibuffer or other modes."
  (interactive)
  (cond
   (mark-active
    (indent-region
     (region-beginning)
     (region-end)))
   ;; At end of word, so *maybe* expand the word
   ((looking-at "\\_>")
    (if (luk-in-comment)
        (dabbrev-expand nil)
      (if company-mode (company-complete) (dabbrev-expand nil))))
   (t (indent-for-tab-command))))

(defun luk-elisp-setup ()
  (add-hook 'emacs-lisp-mode-hook 'luk-elisp--hook)
  (define-key emacs-lisp-mode-map (kbd "C-c g") 'xref-find-definitions-other-window)
  (define-key emacs-lisp-mode-map (kbd "<tab>") 'luk-elisp-smart-tab))
