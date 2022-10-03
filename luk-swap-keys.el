;;; -*- coding: utf-8; lexical-binding: t -*-

(provide 'luk-swap-keys)

(defgroup luk-swap-keys nil "Options for which keys to swap out")
(when (require 'luk nil t) (luk-add-group 'luk-swap-keys))

(defcustom
  luk-swap-keys-disable-arrows
  nil
  "When t `luk-swap-keys-enable' disables the arrow keys"
  :type 'boolean
  :group 'luk-swap-keys)

(defcustom
  luk-swap-keys-move-braces
  nil
  "When t `luk-swap-keys-enable' moves the curly-braces {, } to ö, ä"
  :type 'boolean
  :group 'luk-swap-keys)

(defun luk-swap-keys--use-instead (key)
  (lambda () (interactive) (message "Use %s instead" key)))

(defun luk--global-unset-with-hint (key alternative)
  "Instead of unsetting the key, bind it to a function
that points to what you should be pressing"
  (global-set-key key (luk-swap-keys--use-instead alternative)))

(defun luk--insert-{ () (interactive) (insert "{"))
(defun luk--insert-} () (interactive) (insert "}"))

(defun luk--insert-å () (interactive) (insert "å"))
(defun luk--insert-ä () (interactive) (insert "ä"))
(defun luk--insert-ö () (interactive) (insert "ö"))
(defun luk--insert-Å () (interactive) (insert "Å"))

(defun luk--insert-/ () (interactive) (insert "/"))
(defun luk--insert-back-slash () (interactive) (insert "\\"))

(defun luk-swap-keys-disable ()
  "Undo the settings of luk-swap-keys"
  (interactive)
  (when luk-swap-keys-disable-arrows
    (global-set-key (kbd "<up>") 'previous-line)
    (global-set-key (kbd "<down>") 'next-line)
    (global-set-key (kbd "<left>") 'left-char)
    (global-set-key (kbd "<right>") 'right-char))

  (when luk-swap-keys-move-braces
    (global-set-key (kbd "{") 'self-insert-command)
    (global-set-key (kbd "}") 'self-insert-command)
    (global-set-key (kbd "/") 'self-insert-command)
    (global-set-key (kbd "\\") 'self-insert-command)
    (global-set-key (kbd "å") 'self-insert-command)
    (global-set-key (kbd "Å") 'self-insert-command)
    (global-set-key (kbd "ä") 'self-insert-command)
    (global-set-key (kbd "ö") 'self-insert-command)))

(defun luk-swap-keys-enable ()
  "Swap some key locations.

* Use M-p, M-n to move back, forward by paragraphs.
  The standard C-<up/down> bind moves the hands off the edit-keys, and the alternate
  M-{ is difficult to type on a Swedish keyboard: (Alt+AltGr+7)

When `luk-swap-keys-disable-arrows' is t:
* Disable the arrow keys for navigation, and instead suggest C-p,
  C-n et. al when they are pressed.

When `luk-swap-keys-move-braces' is t:
* move {, } to ö, ä
* move / to å, \\ to Å."
  (interactive)
  (global-set-key (kbd "M-p") 'backward-paragraph)
  (global-set-key (kbd "M-n") 'forward-paragraph)

  (when luk-swap-keys-disable-arrows
    ;; Try to unlearn these :)
    (luk--global-unset-with-hint (kbd "<up>") "C-p")
    (luk--global-unset-with-hint (kbd "<down>") "C-n")
    (luk--global-unset-with-hint (kbd "<left>") "C-b")
    (luk--global-unset-with-hint (kbd "<right>") "C-f"))

  (when luk-swap-keys-move-braces
    (luk--global-unset-with-hint (kbd "{") "ö")
    (luk--global-unset-with-hint (kbd "}") "ä")
    (luk--global-unset-with-hint (kbd "/") "å")
    (luk--global-unset-with-hint (kbd "\\") "Å")
    (global-set-key (kbd "å") 'luk--insert-/)
    (global-set-key (kbd "Å") 'luk--insert-back-slash)
    (global-set-key (kbd "ä") 'luk--insert-})
    (global-set-key (kbd "ö") 'luk--insert-{)

    (global-set-key (kbd "C-ö") 'luk--insert-ö)
    (global-set-key (kbd "C-ä") 'luk--insert-ä)
    (global-set-key (kbd "C-å") 'luk--insert-å)
    (global-set-key (kbd "C-Å") 'luk--insert-Å)))
