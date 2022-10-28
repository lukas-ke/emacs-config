;;; -*- coding: utf-8; lexical-binding: t -*-

;; Modified from http://www.emacsblog.org/2007/03/12/tab-completion-everywhere/

(defun luk-active-snippet-keys ()
  (mapcar (lambda (item) (yas--template-key item))
          (yas--all-templates (yas--get-snippet-tables))))

(defun luk-try-yas-expand ()
  (and
   (bound-and-true-p yas-minor-mode) ; ya-snippet mode available
   (looking-back "@[[:alpha:]]+") ; on word starting with "@"
   (if (yas-expand)
       t ;; expanded as snippet
     ;; Could not expand, maybe the snippet-name is incomplete?
     (save-excursion
       (when (re-search-backward "@[[:alpha:]]+" nil 'noerror)
         ;; Try to complete to a snippet name
         (let ((completion (try-completion (match-string 0) (luk-active-snippet-keys))))
           (if completion
               (progn
                 (replace-match completion)
                 (message "Inserted snippet name: %s" completion)
                 t)
             nil)))))))

(defvar luk-tab-complete-custom nil
  "Function for specializing tab-handling in certain buffers.

When this function is non-nil and returns t,
luk-tab-complete-smart-tab will do no further processing")

(make-variable-buffer-local 'luk-tab-complete-custom)

(defun luk-tab-complete-smart-tab ()
  "Minibuffer compliant smart tab

This smart tab is minibuffer compliant: it acts as usual in the
minibuffer. Else, if mark is active, indents region. Else if
point is at the end of a symbol, expands it. Else indents the
current line.

Warning: This function may be bound to <tab> but not TAB when
used with org-mode. Binding it to TAB risks infinite recursion
due to forwarding to org-cycle and org-cycle falling back to the
TAB-bind. See the documentation for `org-cycle' and
‘org-cycle-emulate-tab’."
  (interactive)
  (unless (and (fboundp luk-tab-complete-custom) (funcall luk-tab-complete-custom))
    (cond
     ((minibufferp)
      (minibuffer-complete))

     (mark-active
      (indent-region
       (region-beginning)
       (region-end)))

     ;; At end of word, so *maybe* expand the word
     ;; TODO: "\\_>" can give some issues because dabbrev-expand can
     ;; insert symbols that prevent \\_> from matching (like .)
     ;; and then further presses of <tab> no longer expands.
     ;; maybe find a different way to check if at end of word, e.g.
     ;; (point) is on space or \n and preceding is not space or \n.
     ((looking-at "\\_>")
      ;; Try to yas-expand
      (when (not (luk-try-yas-expand))
        ;; Not a snippet, try company or dabbrev
        (if (bound-and-true-p company-mode)
            (company-complete)
          (dabbrev-expand nil))))

      ((and (bound-and-true-p company-mode)
            ;; At end of non completely-empty-line in company-mode,
            ;; run company-completion
            (/= (point) (line-beginning-position))
            (= (point) (line-end-position)))
       (company-complete))

      (t
       ;; Not at end of word, just indent-for-tab
       (indent-for-tab-command)))))

  (provide 'luk-tab-complete)
