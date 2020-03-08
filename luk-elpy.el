;;; -*- coding: utf-8; lexical-binding: t -*-

(provide 'luk-elpy)

(defun luk-elpy-setup ()
  (elpy-enable)
  (define-key elpy-mode-map (kbd "C-c g") 'elpy-goto-definition)

  ;; I prefer regular `forward-paragraph' over
  ;; `elpy-nav-forward-block'
  (define-key elpy-mode-map (kbd "C-<down>") nil)
  (define-key elpy-mode-map (kbd "C-<up>") nil)

  ;; Workaround for bug:
  ;; https://github.com/jorgenschaefer/elpy/issues/1381
  ;; (I got something similar in Emacs 26.3 on Windows)
  (setq elpy-eldoc-show-current-function nil))
