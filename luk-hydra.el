;;; -*- coding: utf-8; lexical-binding: t -*-

;; Top level hydra-menu for any mode
;;
;; That is, `luk-hydra/body' shows a menu with simple keys
;; for commands I commonly use.

(require 'hydra)
(require 'cl-lib)

(provide 'luk-hydra)

;;; Mode hydras:
;;; Allow calling the luk-hydra from a mode-specific hydra-menu
;;; and then return to the other menu with '.'
(setq luk-mode-hydra-name nil
      luk-mode-hydra-func nil)

(defun luk-hydra-push (f name)
  "Push a hydra body-function and a name to represent it,
to allow returning to it from the main hydra"
  (setq luk-mode-hydra-func f)
  (setq luk-mode-hydra-name name)
  (luk-hydra/body))

(defun luk-hydra-pop ()
  (interactive)
  "Push a hydra body-function and a name to represent it,
to allow returning to it from the main hydra"
  (when luk-mode-hydra-name
    (let ((PREV-HYDRA luk-mode-hydra-func))
      (setq luk-mode-hydra-name nil)
      (setq luk-mode-hydra-func nil)
      (funcall PREV-HYDRA))))

(defun luk-pop-hydra-str ()
  "Label to show for returning to the mode-hydra (or nothing if
luk-hydra was started directly"
  (if luk-mode-hydra-name
      (format "return to %s\n" luk-mode-hydra-name)
    "quit"))

(defun luk-rename-buffer-and-file ()
  "Rename buffer and file (or just the buffer)
Read the new name from mini-buffer."
  (interactive)
  (let ((OLD-FILE-NAME (buffer-file-name))
        (WAS-MODIFIED (buffer-modified-p)))
    (cond
     (OLD-FILE-NAME
      ;; Rename buffer and file if it exists
      (let* ((NEW-FILE-NAME (read-string "New file name: " OLD-FILE-NAME))
             (NEW-BUFFER-NAME (file-name-nondirectory NEW-FILE-NAME))
             (OLD-EXISTED (file-exists-p OLD-FILE-NAME)))
        (if OLD-EXISTED
            (progn
              (rename-file OLD-FILE-NAME NEW-FILE-NAME)
              (set-buffer-modified-p WAS-MODIFIED))
          (set-visited-file-name NEW-FILE-NAME)

          (set-buffer-modified-p t)
          (message "Warning: Renamed buffer, but visited file does not exist - save buffer to create it."))))

     ((buffer-name)
      ;; Buffer not associated with a file, rename buffer only
      (let* ((NEW-BUFFER-NAME (read-string "New buffer name: " (buffer-name))))
        (rename-buffer NEW-BUFFER-NAME t))))))

(defun luk-delete-file ()
  "Delete the visited file, if any"
  (interactive)
  (let ((FILE-NAME (buffer-file-name)))
    (cond
     ((not FILE-NAME)
      (message "Buffer not associated with a file: \"%s\"." (buffer-name)))
     ((not (file-exists-p FILE-NAME))
      (message "Visited file does not exist:  \"%s\"" FILE-NAME))
     (t
      (when (yes-or-no-p (format "Delete \"%s\"?" FILE-NAME))
        (delete-file FILE-NAME)
        (set-buffer-modified-p t)
        (message "File deleted: \"%s\"." FILE-NAME))))))

(defun luk-copy-file-path ()
  "Add path to current file to kill-ring"
  (interactive)
  (let ((FILE-NAME (buffer-file-name)))
    (when FILE-NAME
      (kill-new FILE-NAME))))

(defun luk-caption (STR)
  (propertize STR 'face 'bold))

(defun luk-find-in-files ()
  (interactive)
  (require 'ag)
  (call-interactively 'ag))

(defhydra luk-hydra (:hint nil)
  (format "\
%s^^^^^^^^^^^^      │ %s^^^^^          │ %s^^^^^^^^^      │ %s^^^^^^              │ %s^^^^^^
^─^─────────────────┼─^─^──────────────┼─^───^────────────┼─^─^───────────────────┼────────
_t_: treemacs       │ _l_: find files  │ _b a_ set        │ _M_ menu bar toggle   │ _c t_ capture todo
_e_: explorer here  │ _f_: .. in files │ _b l_ list       │ _m_ menu bar open     │ _c l_ view captures
_r_: rename         │ ^ ^              │ _b j_ jump       │ _S_ scroll bar toggle │
_D_: delete         │ ^ ^              │ _b J_ jump other │ ^ ^                   │
_C_: copy path      │ ^ ^              │ ^   ^            │ ^ ^                   │

_q_: %s"
          (luk-caption "Current File")
          (luk-caption "Files")
          (luk-caption "Bookmarks")
          (luk-caption "Window")
          (luk-caption "Misc")
          "%s(luk-pop-hydra-str)")

  ("." luk-hydra-pop :exit t)
  ;; Current
  ("t" treemacs :exit t)
  ("e" luk-explore :exit t)
  ("r" luk-rename-buffer-and-file :exit t)
  ("D" luk-delete-file :exit t)
  ("C" luk-copy-file-path :exit t)

  ;; Files
  ("l" luk-list-files :exit t)
  ("f" luk-find-in-files :exit t)

  ;; Bookmarks
  ("b a" bookmark-set :exit t)
  ("b l" list-bookmarks :exit t)
  ("b j" bookmark-jump :exit t)
  ("b J" bookmark-jump-other-window :exit t)

  ;; Window
  ("M" menu-bar-mode :exit nil)
  ("m" menu-bar-open :exit t)
  ("S" scroll-bar-mode :exit nil)
  ("q" luk-hydra-pop :exit t)

  ;; Misc
  ("c t" (org-capture nil "t") :exit t)
  ("c l" org-capture-goto-last-stored :exit t))
