;;; -*- coding: utf-8; lexical-binding: t -*-

(provide 'luk-c++)

(defun luk-c++--mode-hook ()
  ;; Don't indent inside namespace
  (c-set-offset 'innamespace [0]))

(defun luk-c++-setup ()
  (add-hook 'c++-mode-hook 'luk-c++--mode-hook))
