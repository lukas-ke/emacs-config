;;; -*- coding: utf-8; lexical-binding: t -*-

(defun -up (&optional path n)
  "Get the N-th parent folder of PATH or `buffer-file-name'."
  (or path (setq path (buffer-file-name)))
  (or n (setq n 1))
  (dotimes (_num n)
    (setq path (file-name-directory (directory-file-name path))))
  path)


(defun luk-in-comment ()
  "Return t if point is inside a code comment."
  (if (nth 4 (syntax-ppss)) t nil))

(defun luk-new-file-buffer-p ()
  "Return t if the buffer appears to be opened for a new file.

The newness and file-targetting is determined by checking that
the buffer has a file-name for a file that doesn't (yet) exist,
and as an extra check that point is at the start.

This can be used in mode-hooks to check whether the buffer has
just been opened with e.g. `find-file' for a new file, in order
to, say, insert suitable boilerplate for that filetype."
  (and (buffer-file-name) (not (file-exists-p (buffer-file-name))) (= (point) 1)))

(defun luk-capitalize-first-word (string)
  (if (= (length string) 0)
      ""
    (let ((first (substring string nil 1)) (rest (substring string 1)))
      (concat (capitalize first) rest))))

;; luk-save macro

(defun luk--get-save-symbol (other)
  (cond
    ((eq other 'excursion) 'save-excursion)
    ((eq other 'match-data) 'save-match-data)
    ((eq other 'org-outline-visibility) 'org-save-outline-visibility)
    ((eq other 'window-excursion) 'save-window-excursion)
    (t (error "Unknown symbol"))))

(defmacro luk-save (states &rest body)
  "Save each of the STATES; execute body; restore the STATES.

STATES is a list of symbols that correspond to other
save-functions, and can be one or more of the following:

  excursion: `save-excursion'
  match-data: `save-match-data'
  org-outline-visibility: `org-save-outline-visibility'
  window-excursion: `save-window-excursion'

The benefit is that this reduces the nesting in situations where
several of these functions are needed.

Example:
    ;; Position and match-data will be restored on exit
    (luk-save (excursion match-data)
      (goto-char (point-min))
      (re-search-forward \"foo\"))

The above example expands to:
    (save-excursion
      (save-match-data
        (goto-char (point-min)
        (re-search-forward \"foo\"))))"
  ;; Note: I mostly wrote this to practice macros and because some
  ;; other function I wrote had a lot of these save-forms. It might be
  ;; super inefficient or just wrong in other ways.

  ;; Indent only one step on second line (like the example)
  ;; insted of all the way up to the STATES-list.
  (declare (indent defun))

  ;; Build a hierarchy of the function for each symbol, so for
  ;; (excursion match-data) body)
  ;; produce: (save-excursion (save-match-data body))
  (let* ((L (list (luk--get-save-symbol (car states)))) (head L))
    (dolist (state (cdr states))
      (setcdr head (list (list (luk--get-save-symbol state))))
      (setq head (car (cdr head))))
    (setcdr head body)
    L))

(provide 'luk-util)
