;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'luk-markdown)
(require 'luk-util)


;; Test utilities

(setq luk-test-data-dir (concat (file-name-directory (or load-file-name buffer-file-name)) "test-data/"))

(defun luk-test-file (filename)
  (concat luk-test-data-dir filename))

(defun luk-key-file (filename)
  (concat luk-test-data-dir "key-" filename))

(defun luk-run-tests()
  (interactive)
  (ert "luk-"))

(defun luk-test-buffer (test-name &optional prefix suffix)
  (get-buffer-create (concat "*TEST-" (string-join (remove nil (list prefix test-name suffix)) "-") "*")))

(defun luk-test-buffer (test-name &optional prefix suffix)
  (get-buffer-create (concat "*TEST-" (string-join (remove nil (list prefix test-name suffix)) "-") "*")))

(defun luk-test-read-src (test-name)
  (with-temp-buffer
    (insert-file (luk-test-file test-name))
    (buffer-string)))

(defun luk-test-read-key (test-name)
  (with-temp-buffer
    (insert-file (luk-key-file test-name))
    (buffer-string)))

;; Tests for luk-util.el

(ert-deftest luk-save ()
  "Test for function `luk-save'."
  (save-window-excursion
    ;; Setup a buffer
    (switch-to-buffer (luk-test-buffer "luk-save"))
    (erase-buffer)
    (insert "100\n200\naaa\n300\n400\n500\naaa\n600\n")
    (goto-char (point-min))
    (forward-line 4)
    (re-search-forward "aaa") ;; Create match data to preserve
    (when (/= (match-beginning 0) 25) (error "match-beginning wrong in setup"))
    (when (/= (point) 28) (error "point wrong in setup"))

    ;; Run luk-save
    (luk-save (excursion match-data)
      (goto-char (point-min))
      (re-search-forward "aaa")
      (should (= (match-beginning 0) 9)))

    (should (= (match-beginning 0) 25)) ;; Restored?
    (should (= (point) 28)))) ;; Restored?

(ert-deftest luk--up () ;; Prefix with luk- to make selector easy
  "Test for function `-up'."
  (should (string= (-up "c:/plopp/plupp") "c:/plopp/"))
  (should (string= (-up "c:/plopp/plupp" 0) "c:/plopp/plupp"))
  (should (string= (-up "c:/plopp/plupp" 1) "c:/plopp/"))
  (should (string= (-up "c:/plopp/plupp" 2) "c:/"))
  (should (string= (-up "c:/plopp/plupp.txt") "c:/plopp/")))

(ert-deftest luk-capitalize-first-word ()
  (should (string= (luk-capitalize-first-word "hello world") "Hello world"))
  (should (string= (luk-capitalize-first-word "Hello world") "Hello world"))
  (should (string= (luk-capitalize-first-word "4ello world") "4ello world"))
  (should (string= (luk-capitalize-first-word "") "")))

(ert-deftest luk-new-file-buffer-p ()
  (with-current-buffer (find-file (concat luk-test-data-dir "whatever"))
    ;; Buffer visiting a new (unwritten) file
    (should (luk-new-file-buffer-p)))

  (with-current-buffer (find-file (luk-test-file "luk-markdown-delete-trailing-whitespace"))
    ;; Buffer visiting a file that exists
    (should-not (luk-new-file-buffer-p)))

  (with-current-buffer (get-buffer-create "*TEST luk-new-file-buffer-p*")
    ;; Buffer not visiting a file
    (should-not (luk-new-file-buffer-p))))

;; Tests for luk-markdown.el

(ert-deftest luk-markdown-delete-trailing-whitespace ()
  "Test for function `luk-markdown-delete-trailing-whitespace'."
  (let* ((TEST-NAME "luk-markdown-delete-trailing-whitespace")
         (SRC (luk-test-read-src TEST-NAME))
         (KEY (luk-test-read-key TEST-NAME))
         (TRIMMED nil)
         (TEST-BUFFER (luk-test-buffer TEST-NAME)))

    ;; Ensure the src file hasn't been trimmed by mistake already
    (when (string= SRC KEY) (error "Setup failed: SRC and KEY files identical?"))

    ;; Put the trailing whitespace from SRC into a buffer
    (with-current-buffer TEST-BUFFER
      (erase-buffer)
      (insert SRC)
      (markdown-mode)
      (luk-markdown-delete-trailing-whitespace)
      (setq TRIMMED (buffer-string))
      (should (string= TRIMMED KEY)))))


(ert-deftest luk-org-delete-trailing-whitespace ()
  "Test for function `luk-org-delete-trailing-whitespace'."
  (let* ((TEST-NAME "luk-org-delete-trailing-whitespace")
         (SRC (luk-test-read-src TEST-NAME))
         (KEY (luk-test-read-key TEST-NAME))
         (TRIMMED nil)
         (TEST-BUFFER (luk-test-buffer TEST-NAME)))

    ;; Ensure the src file hasn't been trimmed by mistake already
    (when (string= SRC KEY) (error "Setup failed: SRC and KEY files identical?"))

    ;; Put the trailing whitespace from SRC into a buffer
    (with-current-buffer TEST-BUFFER
      (erase-buffer)
      (insert SRC)
      (org-mode)
      (luk-org-delete-trailing-whitespace)
      (setq TRIMMED (buffer-string))
      (should (string= TRIMMED KEY)))))


(ert-deftest luk-def-enum ()
  "Test usage of `luk/def-enum' and related functions."
  ;; For restoring luk/enum-alist after test
  (let ((old-list luk/enum-alist))
    (unwind-protect
        (progn
          (setq luk/enum-alist nil) ;; Use an empty list in test
          (should (= (length luk/enum-alist) 0))

          (luk/def-enum Fruit ('apple 'banana 'orange))
          (luk/def-enum Car ('volvo 'toyota))

          (should (luk/enum-has 'Fruit 'apple))
          (should (not (luk/enum-has 'Fruit 'volvo)))

          (should (= (length luk/enum-alist) 2)) ;; Two enums defined
          (should (equal (assoc 'Fruit luk/enum-alist) '(Fruit apple banana orange)))
          (should (equal (assoc 'Car luk/enum-alist) '(Car volvo toyota)))
          (should (equal (luk/enum-instance 'Car 'volvo) '(Car volvo)))
          (should (eq (luk/enum-type (luk/enum-instance 'Car 'volvo)) 'Car))
          (should (eq (luk/enum-value (luk/enum-instance 'Car 'volvo)) 'volvo))

          ;; Type check
          (should (luk/enum-instance-p '(Car volvo)))
          (should (luk/enum-instance-p '(Fruit apple)))
          (should-not (luk/enum-instance-p '(Car wheelbarrow)))
          (should-not (luk/enum-instance-p '(Foo apple)))

          (should (eq (luk/enum-next-value 'Car 'volvo) 'toyota))

          (let ((debug-on-error nil))
            ;; TODO: If I change this to e.g. 'orange, I only get "aborted" in *ert*?
            ;; (No other failure info)
            (should-error (luk/enum-instance 'Car 'orange))) ;; TODO: Should fail

          ;; Next value
          (should (equal (luk/enum-next '(Car volvo)) '(Car toyota)))

          (should (equal (luk/enum-values 'Car) '(volvo toyota)))

          ;; Wrap around
          (should (equal (luk/enum-next '(Car toyota)) '(Car volvo))))

      ;; Restore
      (setq luk/enum-alist old-list))))

(ert-deftest luk-image-filename-p ()
  (should (luk/image-filename-p "c:/test.png"))
  (should (luk/image-filename-p "file.jpg"))
  (should-not (luk/image-filename-p "c:/test.txt")))
