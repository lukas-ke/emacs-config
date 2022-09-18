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
    (beginning-of-buffer)
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
