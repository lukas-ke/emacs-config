;;; -*- coding: utf-8; lexical-binding: t -*-

(provide 'luk-c++)
(require 'cc-mode)

(defun luk-c++--mode-hook ()
  ;; Don't indent inside namespace
  (c-set-offset 'innamespace [0])

  ;; Don't align line-broken arguments with the parenthesis just
  ;; indent them one step.
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-cont-nonempty '+)
  (c-set-offset 'arglist-cont '0)
  (c-set-offset 'arglist-close 0))


(defun luk-c++-setup ()
  (add-hook 'c++-mode-hook 'luk-c++--mode-hook)

  ;; Open .h files in C++-mode, not c
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

  ;; Hack: Make constexpr indentation work (Needed at least in emacs 26.3)
  (custom-set-variables '(c-noise-macro-names '("constexpr"))))
