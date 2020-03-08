;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'org-bullets) ; https://github.com/sabof/org-bullets
(require 'org)

(provide 'luk-org-mode)

(defun luk-org--descriptive-links (enable)
  "See `org-toggle-link-display'"
  (if enable
      (progn
        (org-remove-from-invisibility-spec '(org-link))
        (setq org-descriptive-links t))
    (progn
      (add-to-invisibility-spec '(org-link))
      (setq org-descriptive-links nil))))

(defun luk-org-toggle-display ()
  """Toggle display of emphasis markers, descriptive links etc."""
  (interactive)
  (let ((show-markers? (not org-hide-emphasis-markers)))
    (if show-markers? (message "Descriptive links, hide emphasis-markers")
      (message "Raw links, show emphasis-markers"))
    (setq org-hide-emphasis-markers show-markers?)
    (luk-org--descriptive-links (not show-markers?))
    (when (eq (not show-markers?) prettify-symbols-mode)
      (call-interactively 'prettify-symbols-mode))
    (org-restart-font-lock)))

(defun luk-org-mode-setup ()
  (with-eval-after-load 'org

    ;;; Keys
    (define-key org-mode-map (kbd "C-c g") 'org-open-at-point)
    (define-key org-mode-map (kbd "C-c a") 'org-agenda)
    (define-key org-mode-map (kbd "<f6>") 'luk-org-toggle-display)

    ;;; Symbol-replacements

    ;; unicode bullets for org-titles instead of asterisks
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

    ;; Use "↴" after folded headings instead of "..."
    (setq org-ellipsis " ↴")

    ;; Use prettify-symbols to get "nicer" checkboxes
    (add-hook
     'org-mode-hook
     (lambda ()
       "Beautify Org Checkbox Symbol"
       (push '("[ ]" . "☐") prettify-symbols-alist)
       (push '("[X]" . "☑" ) prettify-symbols-alist)
       (push '("[-]" . "❍" ) prettify-symbols-alist)
       (prettify-symbols-mode)))

    ;; Bigger check-boxes
    (set-face-attribute 'org-checkbox nil :height 1.5)

    ;; No underline for ↴
    (set-face-attribute 'org-ellipsis nil :underline nil)

    ;; By default, hide emphasis markers like the = and * for
    ;; =verbatim= and *bold* (Toggle with f6)
    (setq org-hide-emphasis-markers t)

    ;; https://stackoverflow.com/questions/40332479/org-mode-folding-considers-whitespace-as-content
    (setq org-cycle-separator-lines -1)

    ;; Open "file:"-links in dired instead of os-application
    (add-to-list 'org-file-apps '(directory . emacs))))
