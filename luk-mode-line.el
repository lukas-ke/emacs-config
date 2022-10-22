;;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'luk-hydra)
(require 'luk-diminish)

(defgroup luk-mode-line nil "Variables for luk-mode-line.")

(defgroup luk-mode-line-faces nil "Faces for luk-mode-line" :group 'luk-mode-line)

(when (require 'luk nil 'noerror)
  (luk-add-group 'luk-mode-line)
  (luk-add-face-group 'luk-mode-line-faces))

(defcustom luk-mode-line-show-major-mode
  nil
  "Whether to show the major mode in the modeline"
  :type 'boolean
  :group 'luk-mode-line)

(defcustom luk-mode-line-show-minor-modes
  nil
  "Whether to show minor modes in the modeline"
  :type 'boolean
  :group 'luk-mode-line)

(defcustom luk-mode-line-show-vc-info
  nil
  "Whether to show version control info in the modeline"
  :type 'boolean
  :group 'luk-mode-line)
(put 'luk-mode-line-show-vc-info 'risky-local-variable t)

(defcustom luk-mode-line-show-buffer-position
  nil
  "Whether to show buffer scroll position in the modeline"
  :type 'boolean
  :group 'luk-mode-line)

(defface luk-mode-line-read-only '((t))
  "Face for read-only indication in mode line"
  :group 'luk-mode-line-faces)

(defface luk-mode-line-modified '((t))
  "Face for modified indication in mode line"
  :group 'luk-mode-line-faces)

(defface luk-mode-line-unmodified
  '((t :family "segoe ui symbol"))
  "Face for not-modified indication in mode line"
  :group 'luk-mode-line-faces)

(defvar luk--mode-line-anzu nil "Whether anzu is available")

(when (require 'anzu nil 'noerror)
  (setq luk--mode-line-anzu t))

;; Magnifying glass indicator for prefixing anzu-search counts in mode-line
(setq luk--mode-line-search (propertize "" 'font-lock-face `(:inherit anzu-mode-line :family "FontAwesome" :height 1)))

(defun luk-anzu-modeline ()
  (if luk--mode-line-anzu
      (when anzu--state (concat luk--mode-line-search (anzu--update-mode-line)))
    ""))

(defvar luk-mode-line-modes-all nil)
(setq luk-mode-line-modes-all
      (list
       `(:propertize ("" mode-name)
		     help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
		     mouse-face mode-line-highlight
		     local-map ,mode-line-major-mode-keymap)
       '("" mode-line-process)
       `(:propertize ("" minor-mode-alist)
		     mouse-face mode-line-highlight
		     help-echo "Minor mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
		     local-map ,mode-line-minor-mode-keymap)
       (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
		   'mouse-face 'mode-line-highlight
		   'local-map (make-mode-line-mouse-map
			       'mouse-2 #'mode-line-widen))
       (propertize " "
                   'font-lock-face `(:family "github-octicons" :height 1.2)
                   'help-echo "Hide minor modes"
		   'mouse-face 'mode-line-highlight
                   'local-map (make-mode-line-mouse-map
			       'mouse-1 #'luk-mode-line-collapse-modes))))
(put 'luk-mode-line-modes-all 'risky-local-variable t)

(defvar luk-mode-line-modes-major nil)
(setq luk-mode-line-modes-major
  (list
   `(:propertize ("" mode-name)
		 help-echo "major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
		 mouse-face mode-line-highlight
		 local-map ,mode-line-major-mode-keymap)
   '("" mode-line-process)
   (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
	       'mouse-face 'mode-line-highlight
	       'local-map (make-mode-line-mouse-map
			   'mouse-2 #'mode-line-widen))

   `(:propertize ""
                 font-lock-face (:family "github-octicons" :height 1.2)
                 help-echo "Hide major mode"
                 mouse-face mode-line-highlight
                 local-map ,(make-mode-line-mouse-map
			     'mouse-1 #'luk-mode-line-collapse-modes))
   `(:propertize ""
                 font-lock-face (:family "github-octicons" :height 1.2)
                 help-echo "Show minor modes"
                 mouse-face mode-line-highlight
                 local-map ,(make-mode-line-mouse-map
			     'mouse-1 #'luk-mode-line-expand-modes))))
(put 'luk-mode-line-modes-major 'risky-local-variable t)

(defvar luk-mode-line-modes-minor nil)
(setq luk-mode-line-modes-minor
      (list
       (propertize ""
               'font-lock-face `(:family "github-octicons" :height 1.2)
               'help-echo "Show major mode"
               'local-map (make-mode-line-mouse-map
			   'mouse-1 #'luk-mode-line-expand-modes))
       `(:propertize ("" minor-mode-alist)
		     mouse-face mode-line-highlight
		     help-echo "Minor mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
		     local-map ,mode-line-minor-mode-keymap)
       (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
		   'mouse-face 'mode-line-highlight
		   'local-map (make-mode-line-mouse-map
			       'mouse-2 #'mode-line-widen))
       (propertize " "
                   'font-lock-face `(:family "github-octicons" :height 1.2)
                   'help-echo "Hide minor modes"
                   'local-map (make-mode-line-mouse-map
			       'mouse-1 #'luk-mode-line-collapse-modes))))
(put 'luk-mode-line-modes-minor 'risky-local-variable t)

(defvar luk-mode-line-separator nil)
(setq luk-mode-line-separator (propertize "│" 'font-lock-face '(:height 1.2 :foreground "#FF00FF")))

(defun luk-mode-line-expand-modes ()
  (interactive)
  (cond
   ((not luk-mode-line-show-major-mode)
    (setq luk-mode-line-show-major-mode t)
    (force-mode-line-update t))
   ((not luk-mode-line-show-minor-modes)
    (setq luk-mode-line-show-minor-modes t)
    (force-mode-line-update t))))

(defun luk-mode-line-collapse-modes ()
  (interactive)
  (if (not luk-mode-line-show-minor-modes)
      (when luk-mode-line-show-major-mode
        (setq luk-mode-line-show-major-mode nil)
        (force-mode-line-update))
    (setq luk-mode-line-show-minor-modes nil)
    (force-mode-line-update)))

(defun luk-mode-line-toggle (symbol)
  (interactive)
  (set symbol (not (eval symbol)))
  (force-mode-line-update t))

(defvar luk-mode-line-show-no-modes
  (propertize ""
              'font-lock-face `(:family "github-octicons" :height 1.2)
              'help-echo "Show major mode"
              'local-map (make-mode-line-mouse-map
			  'mouse-1 #'luk-mode-line-expand-modes))
  "Expander for when all modes are hidden")

(defun luk-mode-line-setup()
  "Mode-line adjustments

TODO: Work in progress."
  (interactive)
  ;; Remove the minimum field width from the default buffer
  ;; identification format.
  ;;
  ;; TODO: The original `mode-line-buffer-identification' did
  ;; some other things too.
  (setq-default mode-line-buffer-identification '(#("%b")))

  (setq-default
   mode-line-format
   '("%e"
     (:eval
      ;; Hack to avoid varying modeline height
      (propertize " " 'face '(:family "segoe ui symbol")))
     ;; mode-line-front-space
     ;; mode-line-mule-info
     ;; mode-line-client
     ;; mode-line-modified
     ;; mode-line-remote
     ;; mode-line-frame-identification

     " "
     ;; Use symbols for read-only and buffer modified (instead of
     ;; `mode-line-mule-info' and `mode-line-modified' which used -%*
     (:eval
      (cond
       (buffer-read-only
        (propertize "🔒"
                    'face 'luk-mode-line-read-only
                    'help-echo "Read only"))
       ((buffer-modified-p)
        (propertize "⯁"
                    'face 'luk-mode-line-modified
                    'face '(:foreground "#ff0000")
                    'help-echo "Buffer has unsaved changes "))
       (t
        (propertize "✓"
                      'face 'luk-mode-line-unmodified
                      'help-echo "Buffer unmodified"))))

     " "
     mode-line-buffer-identification
     " "
     (:eval (when luk-mode-line-show-buffer-position (list mode-line-percent-position " ")))
     (:eval (cond
             ((and line-number-mode column-number-mode) (list 10 "%l,%c"))
             (line-number-mode (list 10 "L%l"))
             (column-number-mode (list 10 "C%c")))
            (line-number-mode (list 10 "%c")))
     (luk-mode-line-show-vc-info (vc-mode vc-mode))
     " "
     (:eval
      (cond
       ((and luk-mode-line-show-major-mode luk-mode-line-show-minor-modes) luk-mode-line-modes-all)
       (luk-mode-line-show-major-mode luk-mode-line-modes-major)
       (luk-mode-line-show-minor-modes luk-mode-line-modes-minor)
       (t luk-mode-line-show-no-modes)))
     mode-line-misc-info
     " "
     ;; Search field for anzu
     (:eval (luk-anzu-modeline))
     " "
     ;; Button for configuring mode line (wrench icon)
     ;; TODO: Right align this
     (:eval
      (propertize "" 'face '(:foreground "#cccccc" :family "FontAwesome")
                  'local-map (make-mode-line-mouse-map
			      'mouse-1 #'luk-mode-line-hydra/body)))
     mode-line-end-spaces)))


;; Mode-line hydra menu (and related functions)

;; TODO: Consider also saving diminish-state (a little tricky
;; since I track if I've diminished, not whether to diminish.
(defun luk-mode-line-custom-save ()
  (interactive)
  (dolist (item '(luk-mode-line-show-minor-modes
                  luk-mode-line-show-major-mode
                  luk-mode-line-show-vc-info
                  luk-mode-line-show-buffer-position
                  line-number-mode
                  column-number-mode
                  luk-should-diminish))
    (customize-set-value item (symbol-value item))
    (customize-mark-to-save item))
  (custom-save-all))

(defhydra luk-mode-line-hydra
  (:hint nil
         :foreign-keys warn
         :pre (setq hydra-amaranth-warn-message "Invalid key (luk-mode-line hydra)")
         :post (setq hydra-amaranth-warn-message luk-hydra-amaranth-original-message)
         :exit nil)
  (format "%s
%%s(luk-hydra-checkbox luk-mode-line-show-minor-modes) _m_ Minor modes          %%s(luk-hydra-checkbox luk-should-diminish) _d_ Diminish
%%s(luk-hydra-checkbox luk-mode-line-show-major-mode) _M_ Major mode
%%s(luk-hydra-checkbox luk-mode-line-show-buffer-position) _p_ Buffer position
%%s(luk-hydra-checkbox line-number-mode) _l_ Line number
%%s(luk-hydra-checkbox column-number-mode) _c_ Column numer
%%s(luk-hydra-checkbox luk-mode-line-show-vc-info) _v_ Version control info
_q_ exit   _S_ save"
          (luk-caption "Show (Mode line)"))
  ("d" (luk-toggle-diminish))
  ("m" (luk-mode-line-toggle 'luk-mode-line-show-minor-modes))
  ("M" (luk-mode-line-toggle 'luk-mode-line-show-major-mode))
  ("v" (luk-mode-line-toggle 'luk-mode-line-show-vc-info))
  ("p" (luk-mode-line-toggle 'luk-mode-line-show-buffer-position))
  ("l" (progn (line-number-mode 'toggle) (force-mode-line-update t)))
  ("c" (progn (column-number-mode 'toggle) (force-mode-line-update t)))
  ("." (luk-hydra-push 'luk-mode-line-hydra/body "Mode line") :exit t)
  ("S" (luk-mode-line-custom-save) :exit t)
  ("q" nil :exit t))

(provide 'luk-mode-line)
