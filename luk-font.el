;;; -*- coding: utf-8; lexical-binding: t -*-

;; Font replacement to support some symbols.

(set-fontset-font "fontset-default" #x1f512 "segoe ui symbol") ;; Padlock: 🔒
(set-fontset-font "fontset-default" #x2bc1 "segoe ui symbol") ;; Diamond: ⯁
(set-fontset-font t '(?😊 . ?😎) "Segoe UI Emoji") ;; Some emoji range)

;; github-octicons installed via all-the-icons
(set-fontset-font t #xf020 "github-octicons") ;;  Git branch
