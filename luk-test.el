;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'luk-markdown)
(require 'luk-util)
(require 'luk-appt)


;; Test utilities

(setq luk-test-data-dir (concat (file-name-directory (or load-file-name buffer-file-name)) "test-data/"))

(setq luk-test-out-dir (concat (file-name-directory (or load-file-name buffer-file-name)) "test-out/"))

(defun luk-test-get-out-dir ()
  (when (not (file-directory-p luk-test-out-dir))
    (make-directory luk-test-out-dir)
    (cl-assert (file-directory-p luk-test-out-dir)))
  luk-test-out-dir)

(defun luk-test-file (filename)
  (concat luk-test-data-dir filename))

(defun luk-key-file (filename)
  (concat luk-test-data-dir "key-" filename))

(defun luk-test-git (dir command &rest args)
  (let ((default-directory dir))
    (let ((result (apply #'call-process "git" nil "*luk-test-git*" nil command args)))
      (when (/= result 0)
        (error (format "Git error code %d" result))))))

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

(ert-deftest luk-📂-up () ;; Prefix with luk- to make selector easy
  "Test for function `-up'."
  (should (string= (📂-up "c:/plopp/plupp") "c:/plopp/"))
  (should (string= (📂-up "c:/plopp/plupp/") "c:/plopp/"))
  (should (string= (📂-up "c:/plopp/plupp" 0) "c:/plopp/plupp"))
  (should (string= (📂-up "c:/plopp/plupp" 1) "c:/plopp/"))
  (should (string= (📂-up "c:/plopp/plupp" 2) "c:/"))
  (should (eq (📂-up "c:/plopp/plupp" 3) nil))
  (should (string= (📂-up "c:/plopp/plupp.txt") "c:/plopp/")))

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


;; luk-appt and helpers

(defun luk-test-create-org-dates ()
  (let ((test-org-file (concat (luk-test-get-out-dir) "test-dates.org")))
    (find-file test-org-file)
    (cl-assert (string= (buffer-file-name) test-org-file))
    (erase-buffer)
    (let* ((now (decode-time))
           (prev-hour (copy-sequence now))
           (next-hour (copy-sequence now))
           (next-hour-plus-five (copy-sequence now))
           (in-ten (copy-sequence now))
           (tomorrow (copy-sequence now)))
      ;; TODO Check if this type of time manipulation handles wrap
      (cl-decf (decoded-time-hour prev-hour) 1)
      (cl-incf (decoded-time-hour next-hour) 1)
      (cl-incf (decoded-time-hour next-hour-plus-five) 1)
      (cl-incf (decoded-time-minute next-hour-plus-five) 5)
      (cl-incf (decoded-time-minute in-ten) 10)
      (cl-incf (decoded-time-day tomorrow) 1)
      (insert (format-time-string "Generated %Y-%m-%d %H:%M\n" (encode-time now)))
      (insert (format-time-string "* <%Y-%m-%d %H:%M> Appointment in the past\n" (encode-time prev-hour)))
      (insert (format-time-string "* <%Y-%m-%d %H:%M> Appointment soon\n" (encode-time next-hour)))
      (insert (format-time-string "* <%Y-%m-%d %H:%M> Appointment tomorrow\n" (encode-time tomorrow)))
      (insert (format-time-string "* <%Y-%m-%d %H:%M> Appointment in 1h 5m\n" (encode-time next-hour-plus-five)))

      ;; Inactive timestamps are not exported to appt
      (insert (format-time-string "* [%Y-%m-%d %H:%M] (Not an) appointment in 10m\n" (encode-time next-hour-plus-five)))

      ;; Timestamps without clock time are not exported to appt
      (insert (format-time-string "* <%Y-%m-%d> (Not an) appointment in 10m\n" (encode-time next-hour-plus-five))))
    (save-buffer)
    test-org-file))

(ert-deftest luk-appt ()
  "Test the ignore and export features of luk-appt."

  ;; For restoring values after test
  (let ((original-org-agenda-files org-agenda-files)
        (original-appt-time-msg-list appt-time-msg-list)
        (original-luk-appt-ignored luk-appt-ignored)
        (original-luk-appt-manually-added luk-appt-manually-added)

        ;; Generate an org-file with dates for exporting to appt
        (test-org-file (luk-test-create-org-dates)))

    (cl-flet
        ;; For verifying entries in the time-msg-list by text-content.
        ;; Uses a regex for the time, and the exact string of the
        ;; message content.
        ((match-meeting (time-msg text)
                        (string-match-p
                         (concat "[0-2][0-9][:][0-5][0-9] " text)
                         (cadr time-msg))))
      (unwind-protect
          (progn
            ;; Clear non-test values
            (setq luk-appt-ignored nil)
            (setq luk-appt-manually-added nil)
            (setq appt-time-msg-list nil)

            ;; Use a generated test file with dates
            (setq org-agenda-files (list test-org-file))
            (luk-export-agenda-filtered)

            ;; Two meetings today
            (should (= (length appt-time-msg-list) 2))
            (should (= (length luk-appt-ignored) 0))
            (should (match-meeting (nth 0 appt-time-msg-list) "Appointment soon"))
            (should (match-meeting (nth 1 appt-time-msg-list) "Appointment in 1h 5m"))

            ;; Ignore the next meeting
            (luk-appt-ignore-next)
            (should (= (length appt-time-msg-list) 1))
            (should (= (length luk-appt-ignored) 1))
            (should (match-meeting (nth 0 appt-time-msg-list) "Appointment in 1h 5m"))

            (let* ((now (decode-time))
                   (soon (copy-sequence now)))
              (cl-incf (decoded-time-minute soon) 20)
              (luk-appt-add-manually (format-time-string "%H:%M" (encode-time soon)) "Manually added"))

            ;; Re-export
            (luk-export-agenda-filtered)
            (should (= (length luk-appt-ignored) 1)) ;; Unmodified
            (should (= (length appt-time-msg-list) 2)) ;; One meeting was filtered
            (should (match-meeting (nth 0 appt-time-msg-list) "Manually added"))
            (should (match-meeting (nth 1 appt-time-msg-list) "Appointment in 1h 5m"))

        ;; Restore
        (setq org-agenda-files original-org-agenda-files)
        (setq appt-time-msg-list original-appt-time-msg-list)
        (setq luk-appt-ignored original-luk-appt-ignored)
        (setq luk-appt-manually-added original-luk-appt-manually-added))))))


(ert-deftest luk-insert-closing-delimiter ()
  (should (string= (luk--opposite-delimiter "(") ")"))
  (should (string= (luk--opposite-string-delimiter ?\")"\""))

  (cl-flet* ((check-next (expected)
                ;; insert one closing delimiter and verify that
                ;; line matches expected
                (luk-insert-closing-delimiter)
                (let ((result (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                  (should (string= result expected))))
             (check (unfinished expected)
                ;; Forward to check-next once if expected is a string,
                ;; otherwise call check-next for each specified item.
                (insert unfinished)
                (if (stringp expected)
                    (check-next expected)
                  (dolist (e expected)
                    (check-next e)))
                (insert "\n")))

    (with-current-buffer (luk-test-buffer "luk-insert-closing-delimiter" nil "-python")
      (erase-buffer)
      (python-mode)
      (check "\"hello\"" "\"hello\"")
      (check "\"\"\"hello" "\"\"\"hello\"\"\"")
      (check "[someFunc({\"1\": \"word"
             '("[someFunc({\"1\": \"word\""
               "[someFunc({\"1\": \"word\"}"
               "[someFunc({\"1\": \"word\"})"
               "[someFunc({\"1\": \"word\"})]")))

    (with-current-buffer (luk-test-buffer "luk-insert-closing-delimiter" nil "-lua")
      (erase-buffer)
      (lua-mode)
      (check "\"hello\"" "\"hello\"")
      (check "[[test" "[[test]]")
      (check "someFunc({1=\"word"
             '("someFunc({1=\"word\""
               "someFunc({1=\"word\"}"
               "someFunc({1=\"word\"})")))))

(ert-deftest luk-buffer-vc-status ()
  (let* ((test-repo-dir (concat (luk-test-get-out-dir) "luk-buffer-vc-status"))
         (file-1 (concat test-repo-dir "/file-1"))
         (file-2 (concat test-repo-dir "/file-2")))
    ;; Initialize a git repository
    (when(file-directory-p test-repo-dir)
      (delete-directory test-repo-dir t nil))
    (make-directory test-repo-dir)
    (luk-test-git test-repo-dir "init")
    (with-temp-buffer (insert "file-1\n") (write-file file-1))
    (luk-test-git test-repo-dir "add" file-1)
    (luk-test-git test-repo-dir "commit" "-m" "commit 1")

    (with-temp-buffer (insert "file-2\n") (write-file file-2))

    (with-current-buffer (find-file-noselect file-1)
      (unwind-protect
          (progn
            (message "%s" (luk/buffer-vc-status))
            (should (equal (luk/buffer-vc-status) '(unmodified up-to-date))))
        (kill-current-buffer)))))
