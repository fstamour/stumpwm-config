;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(in-package #:stumpwm-user)

(getenv "DBUS_SESSION_BUS_PID")

*shell-program*

(defparameter *dbus-variables* (run-shell-command "dbus-launch" t))

(use-package :cl-utilities)


(defun split-sequence-at-first (delimiter sequence)
  "Not efficient"
  (split-sequence delimiter sequence
		  :test (let ((first-p t))
			  (lambda (delimiter current-char) 
			    (when (and (eq delimiter current-char)
				       first-p)
			      (progn
				(setf first-p nil)
				t))))))


(collecting
 (dolist (line (split-sequence #\Newline *dbus-variables*))
   (collect (split-sequence #\= line :count 1))))


(first (split-sequence #\Newline *dbus-variables*))



;;(with-output-to-string (oss)
(split-sequence-at-first #\=
		"DBUS_SESSION_BUS_ADDRESS=unix:abstract=/tmp/dbus-XDRmxRZUJa,guid=d8f902a7f3078edb69bd616e53ff1b7b")






(ql:quickload 'split-sequence)
(ql:quickload 'cl-utilities)
