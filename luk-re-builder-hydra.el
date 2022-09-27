;;; -*- coding: utf-8; lexical-binding: t -*-
(require 'luk-hydra)
(require 'luk-util)

(defun luk-reb-re-syntax-help ()
  "Generates hydra help string for reb-re-syntax."
  (cl-case reb-re-syntax
    ('read "For strings in emacs-lisp source files")
    ('string "For search-and-replace")
    ('rx "Lisp regex-engine")
    (t "?")))

(defun luk-reb-escaped(str)
  "Return the symbol prefixed with escape based on reb-re-syntax."
  (cl-case reb-re-syntax
    ('read (concat "\\\\" str))
    ('string (concat "\\" str))
    ('rx str))) ;; todo: rx

(defun luk-reb-insert-capture-group ()
  "Insert syntax-specific capture-group"
  (cond
   ((member reb-re-syntax '(read string))
    (let ((open (luk-reb-escaped "("))
          (close (luk-reb-escaped ")")))
      (insert open close)
      (backward-char (length close))))
   ((eq reb-re-syntax 'rx)
    (insert "(group )")
    (backward-char))))

(defun luk-reb-insert-brackets ()
  "Insert syntax-specific brackets"
  (cond
   ((member reb-re-syntax '(read string))
    (insert "[]")
    (backward-char))
   ((eq reb-re-syntax 'rx)
    (insert "(any )")
    (backward-char))))

(setq luk-str-oparen "(")

(luk/def-enum Re-syntax ('read 'string 'rx))

(defhydra luk-hydra-re-builder (:hint nil :foreign-keys warn :exit nil)
  "
re-builder
_s_ Syntax: '%-10`reb-re-syntax (%s(luk-reb-re-syntax-help))
_c_ Case sensitive
_(_ Insert group        %s(luk-reb-escaped \"(\")content%s(luk-reb-escaped \")\")
_[_ Insert
_q_: Quit"
  ("<up>" windmove-up)
  ("s" (reb-change-syntax (luk/enum-next-value 'Re-syntax reb-re-syntax)))
  ("(" (luk-reb-insert-capture-group) :exit t)
  ("[" (luk-reb-insert-brackets) :exit t) ;; TODO
  ;; ("c" nil :exit t) ;; TODO
;; TODO: Case sensitive is for target buffer, see reb-toggle-case
  ;;  TODO: Copy regex
  ;; TODO: Add up- (.)
  ("q" nil :exit t)
  ("<return>" nil :exit t))

(global-set-key (kbd "M-,") 'luk-hydra-window/body)

(defun luk-hydra-re-setup-shortcut ()
  (define-key reb-mode-map (kbd "M-.") 'luk-hydra-re-builder/body)
  (define-key reb-lisp-mode-map (kbd "M-.") 'luk-hydra-re-builder/body))

(provide 'luk-re-builder-hydra)
