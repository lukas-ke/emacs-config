;;; -*- coding: utf-8; lexical-binding: t -*-
(require 'luk-util)

(defun luk-find-git-repo (dir)
  (cond ((not dir) (user-error "Not in a git repository"))
        ((and (file-exists-p (expand-file-name ".git/" dir))) dir)
        (t (luk-find-git-repo (-up dir)))))

(defun luk-find-file-in-git-repo ()
  "Find files in the current repository with completion."
  (interactive)
  (find-file
   (completing-read
    "Find repository file: "
    (let ((default-directory (luk-find-git-repo default-directory)))
      (butlast (split-string (shell-command-to-string "git ls-files") "\n"))))))

(provide 'luk-find-file-in-git-repo)
