;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'ring)

(defvar luk-killed-buffer-files nil "Ring for killed buffer file names")

(defun luk-restore-killed-buffer-file ()
  "Restore the most recently killed buffer.

Subsequent invocations will restore the previous killed buffer (up to some limit)."
  (interactive)
  (when (not (ring-p luk-killed-buffer-files))
    (user-error "Killed buffer restoring not activated. \"luk-restore-killed-buffer-setup\" first"))
  (when (= (ring-length luk-killed-buffer-files) 0)
    (user-error "No killed buffer to restore."))
  (let ((f (ring-remove luk-killed-buffer-files 0)))
    (find-file-existing f)
  (message "Reopened \"%s\"" f)))

(defun luk--record-killed-buffer ()
  "Store killed buffer file names to `luk-killed-buffer-files'."
  ;; TODO: Maybe record point and other things too
  (let ((f (buffer-file-name)))
    (when f
      (ring-insert luk-killed-buffer-files f))))

(defun luk-restore-killed-buffer-setup ()
  "Setup recording of killed buffer for restoring.

Adds a hook that records the paths to killed buffers that were
visiting files, so that they can be restored with
`luk-restore-killed-buffer-file'."
  (when (not luk-killed-buffer-files)
    (setq luk-killed-buffer-files (make-ring 5)))
  (add-hook 'kill-buffer-hook 'luk--record-killed-buffer))

(provide 'luk-restore-killed-buffer)
