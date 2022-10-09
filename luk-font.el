;;; -*- coding: utf-8; lexical-binding: t -*-

;; Font replacement to support some symbols.

;; TODO: Should I just set the entire segoe ui symbol private use area?

;; Hmm: "fontset-default" is a fallback fontset
;; https://stackoverflow.com/questions/6083496/how-do-you-specify-a-fallback-font-in-emacs

;; github-octicons installed via all-the-icons
(set-fontset-font t #xf020 "github-octicons") ;;  Git branch

(set-fontset-font "fontset-default" #xE117 "segoe ui symbol") ;; Refresh 
(set-fontset-font "fontset-default" #xE1C9 "segoe ui symbol") ;; Cell phone 
(set-fontset-font "fontset-default" #xE188 "segoe ui symbol") ;; Folder  (private use area)
(set-fontset-font "fontset-default" #xe107 "segoe ui symbol") ;; Trashcan 
(set-fontset-font "fontset-default" #xe105 "segoe ui symbol") ;; Save 
(set-fontset-font "fontset-default" #x1f512 "segoe ui symbol") ;; Padlock: 🔒
(set-fontset-font "fontset-default" #x2bc1 "segoe ui symbol") ;; Diamond: ⯁
(set-fontset-font t #xf121 "FontAwesome") ;; Code icon  (private use area)
(set-fontset-font t #xf046 "FontAwesome") ;; Check-square-o  (private use area)
(set-fontset-font t #xf096 "FontAwesome") ;; Square-o  (private use area)
(set-fontset-font t #xf063 "github-octicons") ;; Quote icon  (private use area)
(set-fontset-font t #xf0a4 "github-octicons") ;; Chevron left  (private use area)
(set-fontset-font t #xe8b8 "Material Icons") ;; Settings  (private use area)
(set-fontset-font t #xe2bc "Material Icons") ;; Attachment 
(set-fontset-font t ?💩 "Segoe UI Emoji") ;; Pile of poo 💩
(set-fontset-font t '(?😊 . ?😎) "Segoe UI Emoji") ;; Some emoji range
(set-fontset-font t #x1f4dd "Segoe UI Emoji") ;; 📝 Memo

;; face-smiling
(set-fontset-font t #x1f600 "Segoe UI Emoji") ;; 😀 Grinning face
(set-fontset-font t #x1f601 "Segoe UI Emoji") ;; beaming face with smiling eyes
(set-fontset-font t #x1f603 "Segoe UI Emoji") ;; 😃 grinning face with big eyes
(set-fontset-font t #x1f604 "Segoe UI Emoji") ;; 😄 grinning face with smiling eyes
(set-fontset-font t #x1f606 "Segoe UI Emoji") ;; grinning squinting face
(set-fontset-font t #x1F605 "Segoe UI Emoji")
(set-fontset-font t #x1F923 "Segoe UI Emoji")
(set-fontset-font t #x1F602 "Segoe UI Emoji")
(set-fontset-font t #x1F642 "Segoe UI Emoji")
(set-fontset-font t #x1F643 "Segoe UI Emoji")
;; (set-fontset-font t #x1FAE0 "Segoe UI Emoji") ;; Melting face not in Segoe UI emoji
(set-fontset-font t #x1F609 "Segoe UI Emoji")
(set-fontset-font t #x1F60A "Segoe UI Emoji")
(set-fontset-font t #x1F607 "Segoe UI Emoji")
(set-fontset-font "fontset-default" #x1f641 "segoe ui emoji") ;; 🙁 Slightly frowning face

;; face-affection
(set-fontset-font t #x1F970 "Segoe UI Emoji")
(set-fontset-font t #x1F60D "Segoe UI Emoji")
(set-fontset-font t #x1F929 "Segoe UI Emoji")
(set-fontset-font t #x1F618 "Segoe UI Emoji")
(set-fontset-font t #x1F617 "Segoe UI Emoji")
;; (set-fontset-font t #x263A  "DejaVu Sans Mono")
(set-fontset-font t #x263A "Segoe UI Emoji")
(set-fontset-font t #x1F61A "Segoe UI Emoji")
(set-fontset-font t #x1F619 "Segoe UI Emoji")
;; (set-fontset-font t #x1F972 "Segoe UI Emoji")

(set-fontset-font t #x1F917 "Segoe UI Emoji")
(set-fontset-font t #x1F92D "Segoe UI Emoji")
;; (set-fontset-font t #x+1FAE2 "Segoe UI Emoji")
;; (set-fontset-font t #x+1FAE3 "Segoe UI Emoji")
(set-fontset-font t #x1F92B "Segoe UI Emoji")
(set-fontset-font t #x1F914 "Segoe UI Emoji")
;; (set-fontset-font t #x+1FAE1 "Segoe UI Emoji")

(set-fontset-font t #x1F44D "Segoe UI Emoji") ;; Thumbs up 👍
(set-fontset-font t #x1F44E "Segoe UI Emoji") ;; Thumbs down 👎

(set-fontset-font t #x1F989 "Segoe UI Emoji") ;; Owl 🦉
