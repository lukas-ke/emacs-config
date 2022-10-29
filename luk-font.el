;;; -*- coding: utf-8; lexical-binding: t -*-

;; Font replacement to support some symbols.

;; TODO: Should I just set the entire segoe ui symbol private use area?

;; Hmm: "fontset-default" is a fallback fontset
;; https://stackoverflow.com/questions/6083496/how-do-you-specify-a-fallback-font-in-emacs

;; Default font
(defun luk-font-available-p (font-name)
  (find-font (font-spec :name font-name)))

(when (not (luk-font-available-p "Symbola"))
  (message "Note: Symbola-font missing, and it is a good font to have."))

(cond
 ((luk-font-available-p "DejaVu Sans Mono")
  (set-face-attribute
   'default
   nil
   :family "DejaVu Sans Mono" ; https://dejavu-fonts.github.io/
   :foundry "outline"
   :slant 'normal
   :weight 'normal
   :height 102
   :width 'normal))
 ((luk-font-available-p "Consolas")
  (set-face-attribute
   'default
   nil
   :family "Consolas")))

(when (luk-font-available-p "Segoe UI Symbol")
  (set-fontset-font "fontset-default" #xE117 "Segoe UI Symbol") ;; Refresh 
  (set-fontset-font "fontset-default" #xE1C9 "Segoe UI Symbol") ;; Cell phone 
  (set-fontset-font "fontset-default" #xE188 "Segoe UI Symbol") ;; Folder  (private use area)
  (set-fontset-font "fontset-default" #xe107 "Segoe UI Symbol") ;; Trashcan 
  (set-fontset-font "fontset-default" #xe105 "Segoe UI Symbol") ;; Save 
  (set-fontset-font "fontset-default" #x1f512 "Segoe UI Symbol") ;; Padlock: 🔒
  (set-fontset-font "fontset-default" #x2bc1 "Segoe UI Symbol") ;; Diamond: ⯁
  )

(when (luk-font-available-p "Segoe UI Emoji")
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
  (set-fontset-font t #x1F609 "Segoe UI Emoji")
  (set-fontset-font t #x1F60A "Segoe UI Emoji")
  (set-fontset-font t #x1F607 "Segoe UI Emoji")
  (set-fontset-font t #x1f641 "Segoe UI Emoji") ;; 🙁 Slightly frowning face
  (set-fontset-font t ?💩 "Segoe UI Emoji") ;; Pile of poo 💩
  (set-fontset-font t '(?😊 . ?😎) "Segoe UI Emoji") ;; Some emoji range
  (set-fontset-font t #x1f4dd "Segoe UI Emoji") ;; 📝 Memo
  ;; (set-fontset-font t #x1FAE0 "Segoe UI Emoji") ;; Melting face not in Segoe UI emoji :(

  ;; face-affection
  (set-fontset-font t #x1F970 "Segoe UI Emoji")
  (set-fontset-font t #x1F60D "Segoe UI Emoji")
  (set-fontset-font t #x1F929 "Segoe UI Emoji")
  (set-fontset-font t #x1F618 "Segoe UI Emoji")
  (set-fontset-font t #x1F617 "Segoe UI Emoji")

  (set-fontset-font t #x263A "Segoe UI Emoji")
  (set-fontset-font t #x1F619 "Segoe UI Emoji")
  (set-fontset-font t #x1F61A "Segoe UI Emoji")
  (set-fontset-font t #x1F92B "Segoe UI Emoji")
  (set-fontset-font t #x1F914 "Segoe UI Emoji")

  (set-fontset-font t #x1F917 "Segoe UI Emoji")
  (set-fontset-font t #x1F92D "Segoe UI Emoji")

  ;; Not sure which group this is in
  (set-fontset-font "fontset-default" #x1f60f "segoe ui emoji") ;; 😏 Smirking face
  (set-fontset-font t #x1F44D "Segoe UI Emoji") ;; Thumbs up 👍
  (set-fontset-font t #x1F44E "Segoe UI Emoji") ;; Thumbs down 👎
  (set-fontset-font t #x1F989 "Segoe UI Emoji") ;; Owl 🦉
  )

(when (luk-font-available-p "FontAwesome")
  ;; FontAwesome installed via all-the-icons.
  ;; Private use area (i.e. a probably bad idea)
  (set-fontset-font t #xf121 "FontAwesome") ;; Code icon  (private use area)
  (set-fontset-font t #xf046 "FontAwesome") ;; Check-square-o  (private use area)
  (set-fontset-font t #xf096 "FontAwesome") ;; Square-o  (private use area)
  )

(when (luk-font-available-p "github-octicons")
  ;; github-octicons installed via all-the-icons.
  ;; Private use area (i.e. a probably bad idea)
  (set-fontset-font t #xf063 "github-octicons") ;; Quote icon  (private use area)
  (set-fontset-font t #xf0a4 "github-octicons") ;; Chevron left  (private use area)
  (set-fontset-font t #xf020 "github-octicons") ;;  Git branch
  )

(when (luk-font-available-p "Material Icons")
  ;; Material Icons installed via all-the-icons.
  ;; Private use area (i.e. a probably bad idea)
  (set-fontset-font t #xe8b8 "Material Icons") ;; Settings  (private use area)
  (set-fontset-font t #xe2bc "Material Icons") ;; Attachment 
  )

