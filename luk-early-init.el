;;; -*- coding: utf-8; lexical-binding: t -*-

;;; Configuration that is best loaded early, to get correct frame
;;; appearance faster, not as a surprise at the end of config loading.
;;
;; Can be loaded before custom-set-variables

;; If this variable isn't bound, load this file from luk-init.el
;; (as a fallback if I haven't added this file to the .emacs)
(setq luk-early-init-performed t)

;; Start in fullscreen
(modify-frame-parameters nil `((fullscreen . fullboth) (t . t)))

;; Disable menu-bar, tool-bar and scroll-bar to get some more space
(customize-set-variable 'menu-bar-mode nil)
(customize-set-variable 'tool-bar-mode nil)
(customize-set-variable 'scroll-bar-mode nil)
