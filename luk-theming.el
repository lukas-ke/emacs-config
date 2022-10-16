;;; -*- coding: utf-8; lexical-binding: t -*-
(require 'luk-hydra)


;; General theme functions

(defvar luk-customized-faces nil
  "Faces changed through `luk-custom-set-faces'.

This will normally be the faces modified by the last
\"post-theme\" function after calling `luk-enable-theme'.

It is used by `luk--reset-themes' to undo the user-customizations
of the current theme before switching to a different theme.")

(defun luk-custom-set-faces (&rest args)
  "Store the face names, then forward to `custom-set-faces'

Store the modified face names in `luk-customized-faces', so that
the customizations can be reverted by `luk--reset-themes'."
  (setq luk-customized-faces (mapcar #'car args))
  (apply #'custom-set-faces args))

(defun luk--reset-themes ()
  "Disable all themes and undo additional face changes.

This function will:
- disable all `custom-enabled-themes'
- undoe the customizations to faces in `luk-customized-faces'.
- clear `luk-customized-faces'"
  (mapc #'disable-theme custom-enabled-themes)
  (set-face-attribute
   'default
   nil
   :family "DejaVu Sans Mono" ; https://dejavu-fonts.github.io/
   :foundry "outline"
   :slant 'normal
   :weight 'normal
   :height 102
   :width 'normal)

  (dolist (face luk-customized-faces)
    (custom-push-theme 'theme-face face 'user 'reset))
  (setq luk-customized-faces nil))

(defun luk-enable-theme (theme)
  "Enables the theme THEME, runs post-configuration if available.

This function enables the theme THEME using `load-theme', and
then runs user-specific post-configuration, if available.

A post-configuration function should be named the same as the
theme-symbol, with prefix \"luk-post\", for example
\"luk-post/my-theme\""
  (interactive)
  (luk--reset-themes)
  (load-theme theme t)

  ;; Update font-lock in org-buffers to hide-leading stars, per
  ;; `org-hide-leading-stars' documentation.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (font-lock-update))))

  ;; Call "luk-post/<theme-name>" function if it is defined, then show
  ;; a message informing that theme loaded OK, and whether the
  ;; post-config function existed or not.
  (let ((theme-message (format "Loaded theme %s." (symbol-name theme)))
        (post-func (intern (concat "luk-post/" (symbol-name theme)))))

    (if (fboundp post-func)
        (progn
          (funcall post-func)
          ;; luk-post/<theme> was called
          (message "%s" (concat theme-message " " (format "Executed %s." (symbol-name post-func)))))

      ;; luk-post/<theme> was not called
      (message "%s"
               (concat theme-message "\n"
                       (format
                        "Post configuration function undefined: %s"
                        (symbol-name post-func)))))))

;; Post configuration functions

(defun luk-post/luk-bright ()
  "Does nothing (I can just modify the theme directly in lukl-bright.el).")

(defun luk-post/zenburn ()
  "Adjust some zenburn theme settings."
  (luk-custom-set-faces
   ;; TODO: Better code-face (e.g. for org-keywords, verbatim)
   ;;       (Maybe I should customize some specific font instead though?)

   ;; Ensure no surrounding font-effects spill over on the
   ;; org-ellipsis
   `(org-ellipsis
     ((t (:underline nil :slant normal :background ,(face-background 'default)))))

   ;; Remove box around checkboxes, since I use prettify-symbols
   `(org-checkbox ((t (:box nil :background ,(face-background 'default)))))

   ;; Weaker color for inactive mode-line
   '(mode-line-inactive ((t (:background "#3F3F3F"))))

   '(luk-mode-line-modified ((t (:foreground "#ff0000"))))
   (let ((fg (cdr (assoc-string "zenburn-blue-2" zenburn-default-colors-alist)))
         (bg (face-background 'fringe)))
     `(bookmark-face ((t (:foreground ,fg :background ,bg)))))))


;; Theme-selection hydra
;; TODO: Consider moving to luk-hydra or elsewhere,
;;       (I should be able to access `luk-enable-theme' before loading
;;       hydra or other packages)
;; TODO: Consider using a list of themes and building the hydra,
;;       (possibly with option for all installed themes)
(defhydra luk-hydra-theme
  (:hint nil :exit nil
         :foreign-keys warn
         :pre (setq hydra-amaranth-warn-message "Invalid key.")
         :post (setq hydra-amaranth-warn-message luk-hydra-amaranth-original-message))
  (format "\
%s
──────────────────────
_1_ luk-bright
_2_ zenburn (low-contrast dark)

_q_ quit"
          (luk-caption "Theme"))
  ("1" (luk-enable-theme 'luk-bright) :exit t)
  ("2" (luk-enable-theme 'zenburn) :exit t)
  ("q" nil :exit t))

(provide 'luk-theming)
