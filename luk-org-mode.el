;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'org-bullets) ; https://github.com/sabof/org-bullets
(require 'org)
(require 'luk-hydra)
(provide 'luk-org-mode)

(defun luk-org--descriptive-links (enable)
  "See `org-toggle-link-display'

This home-brewn variant takes an argument to better support
`luk-org-toggle-display'

Switches between pretty links (enable=true) and full
source (enable=false)."
  (if enable
      (progn
        (org-remove-from-invisibility-spec '(org-link))
        (setq org-descriptive-links t))
    (progn
      (add-to-invisibility-spec '(org-link))
      (setq org-descriptive-links nil))))

(defun luk-org--pretty-entities (enable)
  "See `org-toggle-pretty-entities'

This home-brewn variant takes an argument to better support
`luk-org-toggle-display'

This includes e.g. subscript and superscript and some LaTeX names,
call `org-entities-help for the org documentation."
  (when (not (eq enable org-pretty-entities))
    (setq-local org-pretty-entities enable)
    (when (not org-pretty-entities)
      (save-restriction
        (widen)
        (decompose-region (point-min) (point-max))))))

(defun luk-org-toggle-display ()
  "Toggle display of emphasis markers, descriptive links etc."
  (interactive)
  (let ((pretty-display? (not org-hide-emphasis-markers)))
    (if pretty-display? (message "Descriptive links, hide emphasis-markers")
      (message "Raw links, show emphasis-markers"))
    (setq org-hide-emphasis-markers pretty-display?)
    (luk-org--descriptive-links (not pretty-display?))
    (when (eq (not pretty-display?) prettify-symbols-mode)
      (call-interactively 'prettify-symbols-mode))
    (luk-org--pretty-entities pretty-display?)
    (if pretty-display? (org-display-inline-images) (org-remove-inline-images))
    (org-restart-font-lock)))

(defun luk-org--mode-hook ()
  ;; Use prettify-symbols to get "nicer" checkboxes
  (push '("[ ]" . "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  (prettify-symbols-mode)

  ;; unicode bullets for org-titles instead of asterisks
  (org-bullets-mode 1)

  ;; Use readable links initially
  (luk-org--descriptive-links nil))

(defhydra luk-org-hydra (:hint nil)
  (format "\
%s^^^       %s
^─^──────────────────────────
_p_: toggle raw/pretty
_a_: archive subtree
_l_: org-lint
_q_: quit"
          (luk-caption "Org")
          (luk-caption "[.] for main menu"))
  ("." (luk-hydra-push 'luk-org-hydra/body "org") :exit t)
  ("p" luk-org-toggle-display)
  ("a" org-archive-subtree-default-with-confirmation)
  ("l" org-lint)
  ("q" nil :exit t))

(defun luk-org-summon-hydra ()
  (interactive)
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    ;; E.g. disabling pretty doesn't work well in capture buffer
    (luk-org-hydra/body)))

(defun luk-org-mode-setup ()
  (with-eval-after-load 'org

    ;;; Keys
    (define-key org-mode-map (kbd "C-c g") 'org-open-at-point)
    (define-key org-mode-map (kbd "C-c a") 'org-agenda)
    (define-key org-mode-map (kbd "<f6>") 'luk-org-toggle-display)
    (define-key org-mode-map (kbd "M-.") 'luk-org-summon-hydra)

    ;; Indent on newline
    (define-key org-mode-map (kbd "RET") 'org-return-indent)

    ;; Use "↴" after folded headings instead of "..."
    (setq org-ellipsis " ↴")

    (add-hook 'org-mode-hook 'luk-org--mode-hook)

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
