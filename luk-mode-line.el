(provide 'luk-mode-line)

(defun luk-mode-line-setup()
  "Mode-line adjustments

TODO: Work in progress."
  (interactive)

  ;; Font replacement to support some symbols
  (set-fontset-font "fontset-default" #x1f512 "segoe ui symbol") ;; Padlock: 🔒
  (set-fontset-font "fontset-default" #x2bc1 "segoe ui symbol") ;; Diamond: ⯁
  (set-fontset-font t '(?😊 . ?😎) "Segoe UI Emoji") ;; Some emoji range)

  ;; github-octicons installed via all-the-icons
  (set-fontset-font t #xf020 "github-octicons") ;;  Git branch

  ;; Remove the minimum field width from the default buffer
  ;; identification format.
  ;;
  ;; TODO: The original `mode-line-buffer-identification' did
  ;; some other things too.
  (setq-default mode-line-buffer-identification '(#("%b")))


  (setq-default mode-line-format
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
                     'face '(:foreground "#ff0000")
                     'help-echo "Read only"))
        ((buffer-modified-p)
         (propertize "⯁"
                     'face '(:foreground "#ff0000")
                     'help-echo "Unsaved changes "))
        (t (propertize "✓"
                       'face '(:foreground "#00ff00" (:family "segoe ui symbol"))
                       'help-echo "Unmodified"))))

      " "

      mode-line-buffer-identification
      " "
      mode-line-position
      (vc-mode vc-mode)
      " "
      mode-line-modes
      mode-line-misc-info
      mode-line-end-spaces)))
