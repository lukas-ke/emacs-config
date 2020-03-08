;;; -*- coding: utf-8; lexical-binding: t -*-

;; Provides customization group `luk'.
;; (See its DOC)

(provide 'luk)

(defgroup luk nil "Parent group for my customizable variables.

I add all groups for customizable-variables that I define when in
elisp, so that I can easily find them (or even just remember
that they exist) later.")

(defun luk-add-group (group)
  "Add GROUP to the `luk'-group.

Note: Code should only call this function after verifying that
the `luk'-feature exists, so that the file can be used even if
\"luk.el\" is missing, e.g:

  (when (require 'luk nil t)
    (luk-add-group 'some-new-group))"
  (custom-add-to-group 'luk group 'custom-group))
