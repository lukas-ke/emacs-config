;;; -*- coding: utf-8; lexical-binding: t -*-

;; Font replacement to support some symbols.

;; TODO: Should I just set the entire segoe ui symbol private use area?

(set-fontset-font "fontset-default" #xE117 "segoe ui symbol") ;; Refresh 
(set-fontset-font "fontset-default" #xE1C9 "segoe ui symbol") ;; Cell phone 
(set-fontset-font "fontset-default" #xE188 "segoe ui symbol") ;; Folder 
(set-fontset-font "fontset-default" #xe107 "segoe ui symbol") ;; Trashcan 
(set-fontset-font "fontset-default" #xe105 "segoe ui symbol") ;; Save 
(set-fontset-font "fontset-default" #x1f512 "segoe ui symbol") ;; Padlock: 🔒
(set-fontset-font "fontset-default" #x2bc1 "segoe ui symbol") ;; Diamond: ⯁
(set-fontset-font t ?💩 "Segoe UI Emoji") ;; Pile of poo 💩
(set-fontset-font t '(?😊 . ?😎) "Segoe UI Emoji") ;; Some emoji range

;; github-octicons installed via all-the-icons
(set-fontset-font t #xf020 "github-octicons") ;;  Git branch
