;;; -*- coding: utf-8; lexical-binding: t -*-

;; Modified from http://www.emacsblog.org/2007/03/12/tab-completion-everywhere/

(provide 'luk-tab-complete)

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
  (cond
   ;; TODO: hack. Can I instead make magit override my otherwise
   ;; global key?
   ((string-prefix-p "magit" (symbol-name major-mode))
    (magit-section-toggle (magit-current-section)))

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
    (if (and (eq major-mode 'org-mode) (= ?* (char-before)))
        ;; Don't expand after * in org, it gets weird. Instead let
        ;; org-cycle do its thing.
        ;;
        ;; TODO: Not perfect, if dabbrev expands "word" to "word*",
        ;; tab will instead start cycling which is surprising.
        (org-cycle)

      ;; Try to yas-expand
      (when (not (luk-try-yas-expand))
        ;; Not a snippet, try company or dabbrev
        (message "not a snippet")
        (if (bound-and-true-p company-mode)
            (company-complete)
          (dabbrev-expand nil)))))

   ((eq major-mode 'org-mode)
    ;; Not at end of word in org-mode -> org cycle
    ;; Warning: Can recurse infinitely if `luk-tab-complete-smart-tab'
    ;; is bound to TAB instead of <tab>.
    (org-cycle))

   ((and (bound-and-true-p company-mode)
         (/= (point) (line-beginning-position))
         (= (point) (line-end-position)))
    (company-complete))


   (t
    ;; Not at end of word, just indent-for-tab
    (indent-for-tab-command))))
