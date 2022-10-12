;;; -*- coding: utf-8; lexical-binding: t -*-
(require 'luk-hydra)

;; Theme enable-functionality
(defun luk--reset-themes ()
  (interactive)
  (progn
    (mapc #'disable-theme custom-enabled-themes)

    (set-face-attribute
     'default
     nil
     :family "DejaVu Sans Mono" ; https://dejavu-fonts.github.io/
     :foundry "outline"
     :slant 'normal
     :weight 'normal
     :height 102
     :width 'normal)))

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
  (custom-set-faces
   ;; TODO: Better code-face (e.g. for org-keywords, verbatim)
   ;;       (Maybe I should customize some specific font instead though?)
   ;; TODO: Can I fix org-mode so that stars get hidden immediately
   ;; without `org-mode-restart'?
   '(org-ellipsis ((t (:underline nil :slant normal))))))


;; Theme-selection hydra
;; TODO: Consider moving to luk-hydra or elsewhere,
;;       (I should be able to access `luk-enable-theme' before loading hydra or other packages)
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
