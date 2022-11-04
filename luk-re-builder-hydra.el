;;; -*- coding: utf-8; lexical-binding: t -*-
(require 'luk-hydra)
(require 'luk-util)


;; Formatted strings for hydras
(defun luk-reb-re-syntax-help ()
  "Formats hydra help string for reb-re-syntax."
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

(defun luk-reb-case-fold ()
  "Return string description of case-fold-search"
  (with-current-buffer reb-target-buffer
    (if case-fold-search "Ignore case"
      "Case sensitive")))


;; Insertion functions

(defun luk-reb-insert-capture-group ()
  "Insert capture-group for `reb-re-syntax'."
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
  "Insert `reb-re-syntax'-specific brackets."
  (cond
   ((member reb-re-syntax '(read string))
    (insert "[]")
    (backward-char))
   ((eq reb-re-syntax 'rx)
    (insert "(any )")
    (backward-char))))

;; Enum for cycling `reb-re-syntax'
(luk/def-enum Re-syntax ('read 'string 'rx))


;; Basic insert hydra (C1, `luk-hydra-re-builder-insert')

(defun luk--hydra-re-insert-title ()
  (format "%s" (luk-caption "C-1 Insert Basic")))

(defhydra luk-hydra-re-builder-insert (:hint nil :exit nil :foreign-keys run)
  (format "\
%s
%s
_C-2_ Show Character class insert menu
_C-3_ Show Boundary Anchor insert menu
_*_ match preceeding character zero or more times
_+_ match preceeding character one or more times
_\\^_ Match at beginning of line or invert character set
_$_ Match at end of line
_?_ Match preceding zero or one times (or ungreedy-qualifier after *, ? and +)

_C-q_ Back
"
          (luk--hydra-re-insert-title)
          (make-string (length (luk--hydra-re-insert-title)) ?─))
  ("*" (insert "*"))
  ("+" (insert "+"))
  ("^" (insert "^"))
  ("$" (insert "$"))
  ("?" (insert "?"))
  ("C-q" (luk-hydra-re-builder/body) :exit t)
  ("C-g" (luk-hydra-re-builder/body) :exit t)
  ("C-2" (luk-hydra-re-builder-char-class/body) :exit t)
  ("C-3" (luk-hydra-re-boundary/body) :exit t)

  ;; Overrides for hydra-base-map, do insertion instead
  ("-" (insert "-")) ;; negative argument
  ("0" (insert "0"))
  ("1" (insert "1"))
  ("2" (insert "2"))
  ("3" (insert "3"))
  ("4" (insert "4"))
  ("5" (insert "5"))
  ("6" (insert "6"))
  ("7" (insert "7"))
  ("8" (insert "8"))
  ("9" (insert "9"))
  ("<return>" nil :exit t))

;; Character class hydra (C-2, `luk-hydra-re-builder-char-class/body')

(defun luk--hydra-re-char-class-title ()
  (format "%s" (luk-caption "C-2 Insert Character Class")))

;; TODO: The bracketed classes should go within character groups,
;; so if not within one, one should be inserted.
(defhydra luk-hydra-re-builder-char-class
  (
   :hint nil
         :exit nil
         :foreign-keys warn
         :pre (setq hydra-amaranth-warn-message "This menu only allows character class insertion")
         :post (setq hydra-amaranth-warn-message luk-hydra-amaranth-original-message))
  (format "\
%s
%s
_C-1_ Show basic insert menu
_C-3_ Show boundary insert menu
_a c_ [:ascii:] Any ASCII character (code 0-127)
_a a_ [:alpha:] Any unicode letter
_a n_ [:alnum:] Any unicode letter or digit
_b_   [:blank:] Unicode horizontal whitespace
_c_   [:cntrl:] Control characters (code 0-31)
_d_   [:digit:] Digits
_g_   [:graph:] Graphic characters (most except whitespace, control or weird)
_l_   [:lower:] Lower case letter
_m_   [:multibyte:] Multibyte characters
_n_   [:nonascii:] Non-ASCII character
_p_   [:print:] Any printing character, so whitespace or [:graph:]
_u_   [:punct:] Punctuation
_s_   [:space:] Any character with whitespace syntax for current class-table
_U_   [:unibyte:] Any unibyte character
_P_   [:upper:] Any upper-case character
_w_   [:word:] Any character with word-syntax for current class-table
_x_   [:xdigit:] Hexadecimal digit 0-F
_C-q_ Back"
          (luk--hydra-re-char-class-title)
          (make-string (length (luk--hydra-re-char-class-title)) ?─))
  ("a c" (insert "[:ascii:]"))
  ("a a" (insert "[:alpha:]"))
  ("a n" (insert "[:alnum:]"))
  ("b" (insert "[:blank:]"))
  ("c" (insert "[:cntrl:]"))
  ("d" (insert "[:digit:]"))
  ("g" (insert "[:graph:]"))
  ("l" (insert "[:lower:]"))
  ("m" (insert "[:multibyte:]"))
  ("n" (insert "[:nonascii:]"))
  ("p" (insert "[:print:]"))
  ("u" (insert "[:punct:]"))
  ("s" (insert "[:space:]"))
  ("U" (insert "[:unibyte:]"))
  ("P" (insert "[:upper:]"))
  ("w" (insert "[:word:]"))
  ("x" (insert "[:xdigit:]"))

  ("<left>" #'left-char)
  ("<right>" #'right-char)

  ("C-1" (luk-hydra-re-builder-insert/body) :exit t)
  ("C-3" (luk-hydra-re-boundary/body) :exit t)

  ("C-q" (luk-hydra-re-builder/body) :exit t)
  ("C-g" (luk-hydra-re-builder/body) :exit t)
  ("<return>" nil :exit t))

;; Boundary hydra (C-3, `'luk-hydra-re-boundary/body')

(defun luk--hydra-re-boundary-title ()
  (format "%s" (luk-caption "C-3 Insert Boundary Anchor")))

(defhydra luk-hydra-re-boundary
  (:hint nil
         :exit nil
         :foreign-keys warn
         :pre (setq hydra-amaranth-warn-message "This menu does not allow character insertion") ;; TODO: Maybe it should
         :post (setq hydra-amaranth-warn-message luk-hydra-amaranth-original-message))
  (format "\
%s
%s
_C-1_ Show basic insert menu
_C-2_ Show character class insert menu
_\\^_ Match at beginning of line
_$_ Match at end of line
_`_ Match at beginning of string or buffer
_'_ Match at end of string or buffer
_=_ Match at point (\\=)
_b_ Match at word boundary (\\b)
_C-q_ Back"
          (luk--hydra-re-boundary-title)
          (make-string (length (luk--hydra-re-boundary-title)) ?─))
  ("C-1" (luk-hydra-re-builder-insert/body) :exit t)
  ("C-2" (luk-hydra-re-builder-char-class/body) :exit t)
  ("^" (insert "^"))
  ("$" (insert "$"))
  ("`" (insert "`"))
  ("'" (insert "'"))
  ("=" (insert "\\="))
  ("b" (insert "\\b"))
  ("C-q" nil :exit t))

;; Outer re-builder hydra (`luk-hydra-re-builder/body')

(defun luk--hydra-re-builder-title ()
  (format "%s“%s” (_t_ to retarget)"
          (luk-caption "Regex builder for: ")
          (buffer-name reb-target-buffer)))

;; Outermost re-builder hydra
;;
;; It allows modifying `re-builder' options, inserting some basic
;; constructs and switching to more specific insertion hydras.
(defhydra luk-hydra-re-builder
  (:hint nil
         :foreign-keys warn
         :exit nil
         :pre (setq hydra-amaranth-warn-message "This menu does not allow character insertion")
         :post (setq hydra-amaranth-warn-message luk-hydra-amaranth-original-message))
  (format "\
%s
%s
_C-1_ Show Basic insert menu
_C-2_ Show Character Class insert menu
_C-3_ Show Boundary Anchor insert menu
_s_ Syntax: %%-10`reb-re-syntax (%%s(luk-reb-re-syntax-help))
_c_ Case:   %%s(luk-reb-case-fold)
_(_ Insert group        %%s(luk-reb-escaped \"(\")content%%s(luk-reb-escaped \")\")
_[_ Insert character set
_q_: Quit"
          (luk--hydra-re-builder-title)
          (make-string (length (luk--hydra-re-builder-title)) ?─))
  ("<up>" windmove-up)
  ("s" (reb-change-syntax (luk/enum-next-value 'Re-syntax reb-re-syntax)))
  ("c" (reb-toggle-case))
  ("i" (luk-hydra-re-builder-insert/body) :exit t)
  ("C-1" (luk-hydra-re-builder-insert/body) :exit t)
  ("C-2" (luk-hydra-re-builder-char-class/body) :exit t)
  ("C-3" (luk-hydra-re-boundary/body) :exit t)
  ("(" (luk-reb-insert-capture-group) :exit t)
  ("[" (luk-reb-insert-brackets) :exit t) ;; TODO
  ("<left>" 'backward-char)
  ("<right>" 'forward-char)
  ;; TODO: Copy regex
  ;; TODO: Add up- (.)
  ("C-q" nil :exit t)
  ("q" nil :exit t)
  ("t" #'reb-change-target-buffer)
  ("<return>" nil :exit t))


(defun luk-hydra-re-setup-shortcut ()
  (define-key reb-mode-map (kbd "C-1") 'luk-hydra-re-builder-insert/body)
  (define-key reb-mode-map (kbd "C-2") 'luk-hydra-re-builder-char-class/body)
  (define-key reb-mode-map (kbd "C-3") 'luk-hydra-re-boundary/body))

(add-hook 'reb-mode-hook (lambda () (setq luk-mode-hydra #'luk-hydra-re-builder/body)))

(provide 'luk-re-builder-hydra)
