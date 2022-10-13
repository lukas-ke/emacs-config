;;; -*- coding: utf-8; lexical-binding: t; -*-

(provide 'luk-mode-line)

(defvar luk--mode-line-anzu nil "Whether anzu is available")

(when (require 'anzu nil 'noerror)
  (setq luk--mode-line-anzu t))

;; Magnifying glass indicator for prefixing anzu-search counts in mode-line
(setq luk--mode-line-search (propertize "" 'font-lock-face `(:inherit anzu-mode-line :family "FontAwesome" :height 1)))

(defun luk-anzu-modeline ()
  (if luk--mode-line-anzu
      (when anzu--state (concat luk--mode-line-search (anzu--update-mode-line)))
    ""))

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
      (:eval (luk-anzu-modeline))
      mode-line-end-spaces)))
