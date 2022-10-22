;; -*- coding: utf-8; lexical-binding: t -*-

;;;; luk-menu-bar:
;;
;;; (Buggy) customization of the the behavior for the Emacs menu-bar-mode.
;;;
;;; `luk-menu-bar-show-hide-for-buffer' shows and hides the menu-bar
;;; depending on the predicates in `luk-menu-bar-predicates'.
;;;
;;; `luk-menu-bar-auto-toggle' adds a hook that updates menu-bar
;;; visibility when current-buffer changes.
;;;
;;; Has pretty serious problems with flickering, seemingly
;;; when buffers are switched a lot behind the scenes
;;; (e.g. special buffers?)

(defgroup luk-menu-bar nil "Extensions for the menu-bar-mode")

(when (require 'luk nil 'noerror)
  (luk-add-group 'luk-menu-bar))

(defun luk-menu-toggle-and-select ()
  "Toggle menu-bar-mode, if this results in menu being shown,
focus it to allow selecting an entry"
  (interactive)
  (call-interactively 'menu-bar-mode)
  (when menu-bar-mode
    (menu-bar-open)))

(setq luk-menu-bar-predicate-list '())

;; Keep track of the last-seen buffer to avoid evaluating predicates
;; if the current buffer is unchanged when the
;; `buffer-list-update-hook' is called.
(setq luk-menu-bar-last-buffer "")

(defmacro luk-menu-bar-major-mode-predicate (mode)
  "Define luk-menu-bar-<MODE>-p as a function.
MODE should be a quoted major mode symbol.

The function is a zero-argument predicate which returns true if
the major-mode is MODE. It can be added to
`luk-menu-bar-predicates' to show the menu for this mode.

The function is also added to `luk-menu-bar-predicate-list' so
that it is listed when customizing `luk-menu-bar-predicates'."
  (let ((funsymbol (intern (format "luk-menu-bar-%s-p" (symbol-name (eval mode))))))
    `(progn
       (defun ,funsymbol () (equal major-mode ,mode))
       (add-to-list 'luk-menu-bar-predicate-list ',funsymbol))))

(luk-menu-bar-major-mode-predicate 'package-menu-mode)
(luk-menu-bar-major-mode-predicate 'dired-mode)

(defcustom luk-menu-bar-predicates
  (list 'luk-menu-bar-dired-mode-p)
  "List of functions determining menu-bar visibility.

Used by `luk-menu-bar-show-hide-for-buffer' to determine if the
Emacs menu bar should be shown (`menu-bar-mode').

If any function returns t, the menu bar will be shown, otherwise
it will be hidden."
  :type  '(hook)
  :group 'luk-menu-bar
  :options luk-menu-bar-predicate-list)

(defun luk-menu-bar-any-predicate ()
  "Check predicate functions in `luk-menu-bar-predicates',
return true if any returns true."

  ;; Evaluate with early-return for the first true value (basically "any-of")
  ;;
  ;; Based on `my-some' at
  ;; http://ergoemacs.org/misc/emacs_lisp_some_and_every.html
  (eval `(or ,@(mapcar 'funcall luk-menu-bar-predicates))))

(defun luk-menu-bar-show-hide-for-buffer ()
  "Show/hide the menu-bar depending on current buffer"
  (interactive)
  (if (luk-menu-bar-any-predicate)
      (menu-bar-mode)
    (menu-bar-mode -1)))


(defun luk-menu-bar-show-hide-demoted ()
  ;; Forwards to `luk-menu-bar-show-hide-for-buffer', but demotes
  ;; errors and avoids updates for some cases.
  ;;
  ;; - Errors are demoted (i.e. become messages) since errors on the
  ;; `buffer-list-update-hook' cascade in bad ways.
  ;;
  ;; - No update when in a minibuffer, since that would cause annoying
  ;;   menu-bar showing and hiding when entering a command.
  ;;
  ;; - No update if the current buffer is unchanged since last run
  ;;   (the hook we use runs whenever the buffer-list is modified,
  ;;   e.g. also on reordering and new buffers, even if current-buffer
  ;;   is unchanged).
  (with-demoted-errors
      "Error running `luk-menu-bar-show-hide-for-buffer': %S"
    (unless
	(or
	 (string-prefix-p " *Minibuf" (buffer-name))
	 (string= (buffer-name) luk-menu-bar-last-buffer))
      (luk-menu-bar-show-hide-for-buffer)
      (setq luk-menu-bar-last-buffer (buffer-name)))))

(defun luk-menu-bar-auto-toggle-on ()
  "Enable updating of menu-bar visibility on buffer changes.

Call `luk-menu-bar-show-hide-for-buffer' whenever a buffer is
activated so that the menu-bar is shown or hidden according to
`luk-menu-bar-predicates'."
  (interactive)
  ;; Use the `buffer-list-update-hook', which runs whenever the buffer
  ;; list changes. There doesn't seem to be a hook for current-buffer
  ;; change
  (add-hook 'buffer-list-update-hook 'luk-menu-bar-show-hide-demoted))

(defun luk-menu-bar-auto-toggle-off ()
  (interactive)
  "Disable updating of menu-bar visibility on buffer changes."
  (remove-hook 'buffer-list-update-hook 'luk-menu-bar-show-hide-demoted))

(defun luk-menu-bar--test-predicates ()
  "Outputs a message with the result of evaluating the predicate-list.
Used for testing new predicates."
  (interactive)
  (message "Any: %s" (luk-menu-bar-any-predicate)))

;; Add the customize-group to parent "luk"-group if it exists
(when (require 'luk nil t) (luk-add-group 'luk-menu-bar))

(provide 'luk-menu-bar)
