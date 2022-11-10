;;; -*- coding: utf-8; lexical-binding: t -*-

;; My emacs config, last tested with Emacs 28.1 in Windows 10

(require 'cus-edit) ; For customize-set-variable

;; Make the path to this file (luk-init.el) available as
;; `luk-init-path' when this file is loaded
;; Note: This won't happen on plain eval.
(defvar luk-init-path nil "The path to luk-init.el")
(when load-file-name (setq luk-init-path load-file-name))
(add-to-list 'load-path (concat (file-name-directory luk-init-path) "/others"))

(when (not (boundp 'luk-early-init-performed))
  (message "Loading luk-early-init late :)")
  (load "luk-early-init.el"))

;; Use specific fonts for some characters (e.g. 🔒, 😎)
(load "luk-font.el")

(defun luk-edit-init ()
  "Open my 'luk-init.el'-script in a buffer"
  (interactive) (find-file luk-init-path))

;; Save minibuffer history etc.
(savehist-mode 1)

;; Remember positions in visited files
(save-place-mode 1)

;; Don't beep
(customize-set-variable 'visible-bell t)

;; Start in fullscreen
(modify-frame-parameters nil `((fullscreen . fullboth) (t . t)))

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

;; Don't ask about unsaved files when compiling
(customize-set-variable 'compilation-ask-about-save nil)

;; Do not save any buffers before compiling
(customize-set-variable 'compilation-save-buffers-predicate 'ignore)

;; Always insert spaces as indentation
(setq-default indent-tabs-mode nil)

;; Cursor settings
(blink-cursor-mode -1)

;; Disable auto-save
(customize-set-variable 'auto-save-interval 0) ; Disable character based backup
(customize-set-variable 'auto-save-timeout 0) ; Disable time based backup
(customize-set-variable 'auto-save-default nil)

;; Disable lock-files (#<filename), it seems to mostly cause problems
;; + the files need to be ignored and might clutter folders.
(customize-set-variable 'create-lockfiles nil)

;; Disable backup-files
(setq make-backup-files nil)

;; Make flycheck for emacs-lisp find files on my load path
(setq flycheck-emacs-lisp-load-path 'inherit)

;; No start-up logo screen
(customize-set-variable 'inhibit-startup-message t)
(customize-set-variable 'inhibit-startup-screen t)

;; No initial content (i.e. lisp-comment) in scratch-buffer
(setq initial-scratch-message nil)

(setq
 ;; Preserve text from Windows clipboard before modifying kill-ring
 ;; Without this, it is very easy to lose what you were about to
 ;; paste when it comes from outside emacs.
 ;;
 ;; https://emacs.stackexchange.com/questions/766/add-operating-system-clipboard-to-kill-ring
 save-interprogram-paste-before-kill t)

;; For some reason this message takes a while to appear and delays
;; command execution.
;; TODO: I should test with -q (without my config) to figure out if
;; this is caused by my stuff (seems likely).
(setq suggest-key-bindings nil)

;; Do not suggest shorter M-x invocations. I never manage to commit
;; these to memory anyway.
(setq extended-command-suggest-shorter nil)

(require 'luk-menu-bar)
(global-set-key (kbd "<f10>") 'luk-menu-toggle-and-select)

(if (not (require 'expand-region nil 'noerror))
    (global-set-key (kbd "M-SPC" #'cycle-spacing))

  (defun luk-expand-region-or-cycle-spacing ()
    "Cycle spacing if at space, expand region otherwise."
    (interactive)
    (if (equal last-command 'cycle-spacing)
        (progn
          (setq this-command 'cycle-spacing)
          (cycle-spacing))
      (if (string= " " (buffer-substring-no-properties (point) (+ 1 (point))))
          (progn
            (message "At space")
            (setq this-command 'cycle-spacing)
            (cycle-spacing))
        (er/expand-region 1))))
  (global-set-key (kbd "M-SPC") #'luk-expand-region-or-cycle-spacing))

(global-set-key [(meta return)] 'toggle-frame-fullscreen)

(require 'luk-dired) (luk-dired-setup)

(require 'luk-org-mode) (luk-org-mode-setup)

(require 'luk-magit)
(luk-magit-setup)
(global-set-key (kbd "<f12>") #'luk-magit-status)

;;; Mode for editing .gitignore-files
(require 'gitignore-mode) ; https://github.com/magit/git-modes
(add-to-list 'auto-mode-alist '("\\.gitignore$" . gitignore-mode))

(when (eq system-type 'windows-nt)
  (defun luk-explore ()
    "Open Windows file-browser with current file selected"
    (interactive)
    (if (buffer-file-name)
        (w32-shell-execute
         "open"
         "explorer"
         (concat "/select,\""
                 (replace-regexp-in-string "/" "\\" (buffer-file-name) nil t) "\""))
      (w32-shell-execute "open" "explorer" ".")))

  (global-set-key [(control .)] 'luk-explore))

;; Tab-complete everywhere
;; TODO: Might interfere with e.g. autocomplete or company mode
(require 'luk-tab-complete)

(with-eval-after-load "cus-edit"
  ;; Restore tab-navigation in customize.
  ;; luk-tab-complete eats it, due to cus-edit.el only defining TAB,
  ;; not <tab>.
  (define-key widget-keymap (kbd "<tab>") 'widget-forward)

  ;; Also use tab for next-widget when in editable fields (like the
  ;; search field), it seems unlikely I'd need to insert a tab.
  (define-key custom-field-keymap (kbd "<tab>") 'widget-forward)

  ;; The default keybind, M-tab, isn't usable on windows for
  ;; auto-completing customization widgets.
  (define-key custom-field-keymap (kbd "C-<tab>") 'widget-complete))

(with-eval-after-load 'help-mode
  ;; Restore tab-navigation in help.
  ;; luk-tab-complete eats it, due to help-mode.el only defining TAB,
  ;; not <tab>.
  (define-key help-mode-map (kbd "<tab>") 'forward-button))

;; Note, only binding "<tab>", not "TAB", since org-cycle forwards to
;; the global TAB-bind sometimes, and luk-tab-complete-smart-tab
;; forwards to org-cycle which can lead to infinite recursion
;; https://emacs.stackexchange.com/questions/9631/what-is-the-difference-between-tab-and-tab
(global-set-key (kbd "<tab>") 'luk-tab-complete-smart-tab)

;; Avoid `luk-tab-complete' interfering with tab in ido (for
;; completing current filename with ido-read-filename). ido defines
;; this for key \t, but it seems my global bind for (kbd "<tab>")
;; takes precedence, so explicitly bind (kbd "<tab>") for ido
(with-eval-after-load "ido"
  (define-key ido-common-completion-map (kbd "<tab>") #'ido-complete)
  (add-to-list 'ido-ignore-files "\\`__pycache__/"))

(fido-mode)
(fido-vertical-mode)
(define-key icomplete-fido-mode-map (kbd "C-<return>") #'icomplete-fido-exit)

;; Set syntax for `re-builder' to `read', which requires
;; double-escaping, to build regexps compatible with elisp-code e.g.:
;;   "\\(foo\\|bar\\)"
;;
;; Note: Switch to `string’ with ‘[Control Tab’ in ‘re-builder’ to get
;; simpler syntax compatible with e.g. `re-search-forward', e.g.:
;;   "\(foo|bar\)"
(customize-set-variable 'reb-re-syntax 'read)

;;; Make *compilation*-frame reusable, so that compile is reused in its
;; current frame not a new window in the active frame.
;; (TODO: What?)
(push '("\\*compilation\\*" . (nil (reusable-frames . t))) display-buffer-alist)

;; Trim some whitespace depending on mode
(require 'luk-whitespace)
(add-hook 'before-save-hook 'luk-maybe-trim-whitespace)

(global-set-key[(meta f4)] 'save-buffers-kill-emacs)

(require 'luk-next-error-cycle)
(global-set-key (kbd "C-+") #'luk-next-error-cycle)

(require 'luk-elisp) (luk-elisp-setup)

(require 'luk-c++) (luk-c++-setup)

(require 'luk-markdown) (luk-markdown-setup)

(require 'luk-swap-keys) (luk-swap-keys-enable)

(defun luk-european-calendar () (calendar-set-date-style 'european))
(add-hook 'calendar-load-hook 'luk-european-calendar)

;; Enable the function narrow-to-region without confirmation
(put 'narrow-to-region 'disabled nil)

(defun luk-ask-create-dir ()
  "Ask to create the folder for the current file if it doesn't exist"
  (when buffer-file-name
    (let ((dir (file-name-directory buffer-file-name)))
      (when (and (not (file-exists-p dir))
                 (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
        (make-directory dir t)))))
(add-hook 'before-save-hook 'luk-ask-create-dir)

;; English weekdays in the time stamps of Org mode
;; files and in the agenda.
;; TODO: Not needed?
(setq system-time-locale "C")

(require 'iy-go-to-char)
(global-set-key (kbd "M-m") 'iy-go-to-char)

(require 'luk-appt)
(luk-appt-setup)

(require 'luk-hydra)
(require 'luk-theming) ;; TODO: Needed for luk-hydra to find theme-hydra
(global-set-key (kbd "M-.") 'luk-hydra-summon)
(global-set-key (kbd "M-,") #'luk-show-mode-hydra)
(global-set-key (kbd "M--") #'luk-show-context-hydra)
(global-set-key (kbd "C-,") #'luk-show-window-hydra)
(global-set-key (kbd "M-ö") #'luk-settings-hydra/body)

(require 'luk-mode-line)
(luk-mode-line-setup)

;; Auto-center the content by adjusting margins when using a single
;; emacs-window.
(require 'perfect-margin)
(setq perfect-margin-hide-fringes nil)
(setq perfect-margin-visible-width 120)
;; It is annoying when images are truncated by the margins, so exclude
;; them from perfect-margin-mode.
(add-to-list 'perfect-margin-ignore-modes 'image-mode)
(perfect-margin-mode 1)

(defun luk-edit-image (path)
  (when (or (not luk-image-editor) (= 1 (length luk-image-editor)))
    (user-error "Error: luk-image-editor not set"))
  (interactive "fEdit image file: ")
  (start-process "image-editor" nil luk-image-editor path))

(defun luk-edit-buffer-image ()
  (interactive)
  (unless (buffer-file-name)
    (user-error "Buffer not associated with a file."))
  (luk-edit-image (buffer-file-name)))

(with-eval-after-load 'image-mode
  ;; Bind e and E to edit (e is more convenient, E matches my org-mode
  ;; image context hydra)
  (define-key image-mode-map (kbd "e") #'luk-edit-buffer-image)
  (define-key image-mode-map (kbd "E") #'luk-edit-buffer-image))

(defcustom luk-image-editor
  ""
  "External image editor"
  :type 'file
  :group 'luk)

(defun luk-select-window-at-mouse-position()
  "Select the window the mouse pointer is over"
  (interactive)
  (let ((pos (mouse-position)))
    (select-window (window-at (cadr pos)
                              (cddr pos)
                              (car pos)))))

;; With perfect-margin, a lot of the window can become the margin, so
;; let clicks in the margin select windows too, not just the editable
;; area.
(global-set-key (kbd "<left-margin> <mouse-1>") 'luk-select-window-at-mouse-position)
(global-set-key (kbd "<right-margin> <mouse-1>") 'luk-select-window-at-mouse-position)

(autoload 'luk-list-files "luk-list-files" nil t)

(when (require 'lsp-mode nil 'noerror)
  (require 'luk-lsp-conf)
  (luk-lsp-conf-setup))

;; Enable shift + up, right, down, left for moving between windows.
;;
;; Note: Not using (windmove-default-keybindings), I couldn't figure
;; out how to get org-modes cycling to take precedence then.
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<right>") 'windmove-right)

;; Unset C-x o, to retrain me to use M-o
(global-unset-key (kbd "C-x o"))

(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "M-o") 'other-window))

;; - C-M-a goes to the beginning of a function definition,
;; - C-M-e should go to the end of one ...
;; ... but at least on my laptop, the key-sequence C-M-e gets replaced
;; with the euro sign (i.e. Alt+gr e). Since I rarely type that, just
;; bind the symbol to end-of-defun so C-M-e behaves as expected.I will
;; undoubtedly be super-confused if I ever try to type € though.
(global-set-key (kbd "€") 'end-of-defun)

;; Easier to remember than the defaults C-c b, C-c f or l, r
(define-key help-mode-map (kbd "b") 'help-go-back)
(define-key help-mode-map (kbd "f") 'help-go-forward)

(if (require 'form-feed nil 'noerror)
    ;; Display lines instead of "^L" for page-break in help-buffers
    (add-hook 'help-mode-hook 'form-feed-mode))

(with-eval-after-load 're-builder
  (require 'luk-re-builder-hydra)
  (add-hook
   'reb-mode-hook
   'luk-hydra-re-setup-shortcut))

;; Calendar
(setq calendar-week-start-day 1) ;; Use monday as first day of week
(setq calendar-date-style 'iso)

(with-eval-after-load "bookmark"
  ;; Replace the “bookmark-fringe-mark-bitmap” circle defined by the
  ;; bookmark-package with a classic bookmark icon.
  (define-fringe-bitmap 'bookmark-fringe-mark
    "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe7\xc3\x81"
    12 8 'center))

(when (require 'yasnippet nil 'noerror)
  (setq yas-snippet-dirs (list (concat (file-name-directory luk-init-path) "snippets")))
  ;; I use global tab-binds for `luk-tab-complete-smart-tab' instead,
  ;; so disable the yas-snippet binds
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (yas-global-mode 1))

;; Go to the dired-folder for the open file on M-up
(global-set-key (kbd "M-<up>") #'dired-jump)

;; Close ([{" on "¤" (s-4 on my keyboard).
(require 'luk-insert-closing-delimiter)
(global-set-key (kbd "¤") #'luk-insert-closing-delimiter)

(when (require 'anzu nil 'noerror)
  (setq anzu-cons-mode-line-p nil)
  (setq anzu-mode-lighter "")
  (global-anzu-mode +1))

(require 'luk-diminish)
(luk-maybe-diminish)

;; Lower tooltip delay from the default (0.7)
(setq tooltip-delay 0.1)

;; which-function-mode can put super-long stuff on mode-line
(defun luk-which-func-cleanup (str)
  (truncate-string-to-width str 25 nil nil ".."))
(setq which-func-cleanup-function #'luk-which-func-cleanup)

(when (require 'minibuffer-line nil 'noerror)
  (require 'luk-idle-minibuffer-line)
  (luk-idle-minibuffer-line-enable))

;; Hide the extra-help in the completions buffer
(setq completion-show-help nil)


;; Make built in elisp files read-only, to avoid mistaken modification

(defun luk-built-in-lisp-dir ()
  "Find the lisp/ directory in the emacs installation.

Done by finding the 'emacs-version file (surely there's a better
way?)."
  (let* ((buf (car (find-definition-noselect 'emacs-version nil)))
         (lisp-dir (directory-file-name (file-name-directory
                               (buffer-file-name buf)))))
    (kill-buffer buf)
    ;; TODO: Maybe add sanity check that this is the correct lisp/-dir?
    lisp-dir))

(defun luk-make-built-in-read-only ()
  "Make built-in emacs-lisp files read-only."

  ;; Create 'read-only "directory class"
  (dir-locals-set-class-variables
   'read-only
   '((nil . ((buffer-read-only . t)))))

  ;; Apply the class to a list of directories.
  (dolist (dir
           (list
            ;; TODO: Consider adding ~/.emacs.d/elpa/ folder, but
            ;;   then it needs to be made readable on package install etc.
            ;;   (and maybe in other situations, for example might
            ;;   some packages store data there?
            (luk-built-in-lisp-dir)))
    (dir-locals-set-directory-class (file-truename dir) 'read-only)))
(luk-make-built-in-read-only)


(require 'recentf)
(require 'luk-find-x)
(recentf-mode t)
(setq recentf-max-saved-items 100)

;; Add luk-find-file-in-git-repo on C-x f as an alternative to
;; `find-file' (note on C-x C-f).
(global-set-key (kbd "C-x f") #'luk-find-file-in-git-repo)

(global-set-key (kbd "C-x k") #'kill-current-buffer)
(global-set-key (kbd "C-x C-r") #'luk-recentf-open)


;; Restore killed buffers on C-s-t
(require 'luk-restore-killed-buffer)
(luk-restore-killed-buffer-setup)
(global-set-key (kbd "C-S-t") #'luk-restore-killed-buffer-file)


;; https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

      Move point to the first non-whitespace character on this line.
      If point is already there, move to the beginning of the line.
      Effectively toggle between the first non-whitespace character and
      the beginning of the line.

      If ARG is not nil or 1, move forward ARG - 1 lines first.  If
      point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)

(setq delete-by-moving-to-trash t)

(if (require 'crux nil 'noerror)
    (progn
      (global-set-key (kbd "C-x 4 t") #'crux-transpose-windows)
      (global-set-key (kbd "M-o") #'crux-other-window-or-switch-buffer))

  ;; More convenient bind M-o (than C-x o) for cycling windows
  (global-set-key (kbd "M-o") 'other-window))

(require 'luk-diff)
(luk-ediff-setup)


;; Allow toggling if certain operations target current or other window


(global-set-key (kbd "C-x o") #'toggle-luk-other)
(global-set-key (kbd "C-x C-f") #'luk-find-file)
(global-set-key (kbd "C-x b") #'luk/read-and-switch-to-buffer)

(require 'luk-help-mode)
(luk-help-mode-setup)

(advice-add #'save-some-buffers :before #'luk/clear-buffers-modified-if-not)


(provide 'luk-init)
