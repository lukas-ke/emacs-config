;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'image-file)


;; Miscellaneous functions

(defun 📂-up (&optional path n)
  "Get the N-th parent folder of PATH or `buffer-file-name'."
  (unless path (setq path (buffer-file-name)))
  (unless n (setq n 1))
  (if (= n 0) path
    (let ((new-path path)
          (prev-path nil))
      (cl-dotimes (_num n new-path)
        (setq prev-path new-path)
        (setq new-path (file-name-directory (directory-file-name new-path)))
        (when (string= new-path prev-path)
          (setq new-path nil)
          (cl-return))))))


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

(defun luk/image-filename-p (filename)
  (-any (lambda (ext) (string-suffix-p (concat "." ext) filename)) image-file-name-extensions))

(defun luk-find-git-repo (dir &optional noerror)
  (cond ((not dir)
         (if noerror
             nil
           (user-error "Not in a git repository")))
        ((and (file-exists-p (expand-file-name ".git/" dir))) dir)
        (t (luk-find-git-repo (📂-up dir) noerror))))

(defun luk/buffer-vc-status ()
  (interactive)
  (let* ((file-name (buffer-file-name))
         (exists (if file-name (file-exists-p file-name)))
         (vc-dir (luk-find-git-repo default-directory 'noerror))
         (state (if (and file-name vc-dir)
                    (vc-state file-name)
                  nil)))
    (cond
     ((not file-name) '(non-file not-versioned))
     ((not vc-dir)
      (cond ((not exists) '(missing not-versioned))
            ((buffer-modified-p) '(modified not-versioned))
            (t '(unmodified not-versioned))))
     (state
      (cond ((and (not exists) (eq state 'up-to-date))
             '(missing deleted))
            ;; TODO: Transform all states to a shorter set
            (t (list (if (buffer-modified-p) 'modified 'unmodified) state))))
     ((and file-name vc-dir exists) (list (if (buffer-modified-p) 'modified 'unmodified) 'new))
     ((and file-name vc-dir) 'new-unsaved))))




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


;; Enumerations

(defvar luk/enum-alist nil "Association list of enumerations.

Enumeration definitions created with luk/def-enum are appended to
this list.")

;; TODO: Make repeated invocations update the symbol
(defmacro luk/def-enum (name values)
  "Define a new enumeration with NAME and VALUES."
  (list 'push `(list (quote ,name) ,@values) 'luk/enum-alist))

(defun luk/enum-has (enum-name value)
  "Return t if the enum ENUM-NAME has VALUE as an alternative."
  (if (member value (cdr (assoc enum-name luk/enum-alist))) t nil))

(defun luk/enum-instance-p (object)
  "Return t if OBJECT is an enum instance"
  (and
   (listp object)
   (= 2 (length object))
   (assoc (car object) luk/enum-alist)
   (luk/enum-has (car object) (cadr object))))

(defun luk/enum-instance (enum-symbol value)
  "Instantiate ENUM-SYMBOL with VALUE.

VALUE must be a valid value for the enum denoted by ENUM-SYMBOL."
  (cl-assert (luk/enum-has enum-symbol value) nil (format "Invalid value for enum %s: %s" enum-symbol value))
  (list enum-symbol value))

(defun luk/enum-type (instance)
  "Return the enum-type of INSTANCE."
  (car instance))

(defun luk/enum-value (instance)
  "Return the value of the enum INSTANCE."
  (cadr instance))

(defun luk/enum-values (enum-name)
  "Return the possible values for ENUM-NAME."
  (cdr (assoc enum-name luk/enum-alist)))

(defun luk/enum-next (instance)
  "Return a new instance with a value one step ahead of INSTANCE.

INSTANCE should be an enum-instance, that is
    (<enum-symbol> <enum-value>)"
  (let ((values (cdr (assoc (car instance) luk/enum-alist))))
    (list (car instance)
          (nth (% (+ 1 (cl-position (luk/enum-value instance) values)) (length values))
               values))))

(defun luk/enum-next-value (enum-name value)
  "Return the value after VALUE for the ENUM-NAME enum.

VALUE should be a valid value for the specified enum."
  (let ((values (cdr (assoc enum-name luk/enum-alist))))
    (nth (% (+ 1 (cl-position value values)) (length values))
         values)))

(provide 'luk-util)
