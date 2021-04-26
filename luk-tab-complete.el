;;; -*- coding: utf-8; lexical-binding: t -*-

;; Modified from http://www.emacsblog.org/2007/03/12/tab-completion-everywhere/

(provide 'luk-tab-complete)

(defun luk-tab-complete-smart-tab ()
  "Minibuffer compliant smart tab

This smart tab is minibuffer compliant: it acts as usual in the
minibuffer. Else, if mark is active, indents region. Else if
point is at the end of a symbol, expands it. Else indents the
current line."
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
      (dabbrev-expand nil)))

   ((eq major-mode 'org-mode)
    ;; Not at end of word in org-mode -> org cycle
    (org-cycle))

   (t
    ;; Not at end of word, just indent-for-tab
    (indent-for-tab-command))))
