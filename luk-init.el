;;; -*- coding: utf-8; lexical-binding: t -*-

;; My emacs config, last tested with Emacs 26.3 in Windows 10

(require 'cus-edit) ; For customize-set-variable

;; Make the path to this file (luk-init.el) available as
;; `luk-init-path' when this file is loaded
;; Note: This won't happen on plain eval.
(when load-file-name (setq luk-init-path load-file-name))

(defun luk-edit-init ()
  "Open my 'luk-init.el'-script in a buffer"
  (interactive) (find-file luk-init-path))

;; Don't beep
(customize-set-variable 'visible-bell t)

;; Allow short y/n for all yes-no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Consider a single space to be a sentence separator for e.g
;; fill-paragraph (M-q)
(customize-set-variable 'sentence-end-double-space nil)

;; Coding system (utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Enable pasting unicode into emacs on Windows, otherwise I get "?"
;; instead for high code point symbols (e.g. copied from a web page).
(when (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-16-le))

;; Auto-revert behavior
(customize-set-variable 'auto-revert-use-notify nil)
(customize-set-variable 'auto-revert-verbose nil)

;; Display column number in mode-line
(customize-set-variable 'column-number-mode t)

;; Don't ask about unsaved files when compiling
(customize-set-variable 'compilation-ask-about-save nil)

;; Do not save any buffers before compiling
(customize-set-variable 'compilation-save-buffers-predicate 'ignore)

(setq-default indent-tabs-mode nil)

;; Cursor settings
(set-cursor-color "#ffffff")
(blink-cursor-mode -1)

;; Disable lock-files (#<filename), it seems to mostly cause problems
;; + the files need to be ignored and might clutter folders.
(customize-set-variable 'create-lockfiles nil)

;; Disable auto-save
(customize-set-variable 'auto-save-interval 0) ; Disable character based backup
(customize-set-variable 'auto-save-timeout 0) ; Disable time based backup
(customize-set-variable 'auto-save-default nil)

;; Disable backup-files
(setq make-backup-files nil)

;; No start-up logo screen
(customize-set-variable 'inhibit-startup-message t)
(customize-set-variable 'inhibit-startup-screen t)

;; Disable menu-bar, tool-bar and scroll-bar to get some more space
(customize-set-variable 'menu-bar-mode nil)
(customize-set-variable 'tool-bar-mode nil)
(customize-set-variable 'scroll-bar-mode nil)

;; No initial content (i.e. lisp-comment) in scratch-buffer
(setq initial-scratch-message nil)

(setq
 ;; Preserve text from Windows clipboard before modifying kill-ring
 ;; Without this, it is very easy to lose what you were about to
 ;; paste when it comes from outside emacs.
 ;;
 ;; https://emacs.stackexchange.com/questions/766/add-operating-system-clipboard-to-kill-ring
 save-interprogram-paste-before-kill t)

(setq
 ;; The compacting of font-caches made "unicode.txt" slow to navigate.
 ;;
 ;; Note: See discussion for why this shouldn't normally be set:
 ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27350
 inhibit-compacting-font-caches t)

;; Default font
(set-face-attribute
 'default
 nil
 :family "DejaVu Sans Mono" ; https://dejavu-fonts.github.io/
 :foundry "outline"
 :slant 'normal
 :weight 'normal
 :height 102
 :width 'normal)

(require 'luk-menu-bar)
(global-set-key (kbd "<f10>") 'luk-menu-toggle-and-select)

;; Add find-file-in-git-repo on [C-x f] as an alternative to
;; `find-file' (note on [C-x C-f]).
;; http://github.com/re5et/find-file-in-git-repo
;;
;; This finds file names in the entire repository, and allows
;; selecting among multiple-matches with [left], [right] using
;; ‘ido.el’.
(require 'find-file-in-git-repo) ; http://github.com/re5et/find-file-in-git-repo
(global-set-key (kbd "C-x f") 'find-file-in-git-repo)

(global-set-key [(meta return)] 'toggle-frame-fullscreen)

(require 'luk-dired) (luk-dired-setup)

(require 'luk-org-mode) (luk-org-mode-setup)

(require 'luk-magit) (luk-magit-status-set-key [f12])

;;; Mode for editing .gitignore-files
(require 'gitignore-mode) ; https://github.com/magit/git-modes
(add-to-list 'auto-mode-alist '("\\.gitignore$" . gitignore-mode))

;; Open Windows file-browser in folder of current file on "C-."
(defun luk-explore()
  (interactive)
  (start-process "luk_explorer" nil "explorer" "." ))
(global-set-key [(control .)] 'luk-explore)

;; Tab-complete everywhere
;; TODO: Might interfere with e.g. autocomplete or company mode
(require 'luk-tab-complete)
(global-set-key [(tab)] 'luk-tab-complete-smart-tab)

;; Set syntax for `re-builder' to `read', which requires
;; double-escaping, to build regexps compatible with elisp-code e.g.:
;;   "\\(foo\\|bar\\)"
;;
;; Note: Switch to `string’ with ‘[Control Tab’ in ‘re-builder’ to get
;; simpler syntax compatible with e.g. `re-search-forward', e.g.:
;;   "\(foo|bar\)"
(customize-set-variable 'reb-re-syntax 'read)

;;; Open .h files in C++-mode, not c
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;; Autoload for xah-find-functions (find in files)
;; https://github.com/xahlee/xah-find
(autoload 'xah-find-text "xah-find" "find replace" t)
(autoload 'xah-find-text-regex "xah-find" "find replace" t)
(autoload 'xah-find-replace-text "xah-find" "find replace" t)
(autoload 'xah-find-replace-text-regex "xah-find" "find replace" t)
(autoload 'xah-find-count "xah-find" "find replace" t)

;;; Make *compilation*-frame reusable, so that compile is reused in its
;; current frame not a new window in the active frame.
;; (TODO: What?)
(push '("\\*compilation\\*" . (nil (reusable-frames . t))) display-buffer-alist)

;; Delete trailing whitespace on save (except in markdown where two
;; trailing spaces mean linebreak).
(defun maybe-delete-trailing-whitespace()
  (if (not (string= major-mode "markdown-mode"))
      (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'maybe-delete-trailing-whitespace)

(global-set-key[(meta f4)] 'save-buffers-kill-emacs)

(require 'luk-next-error-cycle)
(global-set-key [(control +)] 'luk-next-error-cycle)

;; Elpy mode for all .py-buffers
(when (require 'elpy nil 'noerror) (require 'luk-elpy) (luk-elpy-setup))

(require 'luk-elisp) (luk-elisp-setup)

(require 'luk-c++) (luk-c++-setup)

(require 'luk-markdown) (luk-markdown-setup)

(require 'luk-swap-keys) (luk-swap-keys-enable)

(add-hook
 'calendar-load-hook
 (lambda ()
   (calendar-set-date-style 'european)))

;; When saving a file in a directory that doesn't exist, offer to
;; (recursively) create the file's parent directories."
(add-hook
 'before-save-hook
 (lambda ()
   (when buffer-file-name
     (let ((dir (file-name-directory buffer-file-name)))
       (when (and (not (file-exists-p dir))
                  (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
         (make-directory dir t))))))

;; English weekdays in the time stamps of Org mode
;; files and in the agenda.
;; TODO: Not needed?
(setq system-time-locale "C")

(toggle-frame-fullscreen) ; Start in fullscreen
