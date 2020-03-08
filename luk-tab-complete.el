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
  (if (string-prefix-p "magit" (symbol-name major-mode))
      ;; TODO: hack. Can I instead make magit override my otherwise global key?
      (magit-section-toggle (magit-current-section))
    (if (minibufferp)
        (minibuffer-complete)
      (if mark-active
          (indent-region (region-beginning)
                         (region-end))
        (if (looking-at "\\_>")
            (dabbrev-expand nil)
          (indent-for-tab-command))))))
