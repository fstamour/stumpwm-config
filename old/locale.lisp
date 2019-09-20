;;;;
;;;; Depends on setxkbmap
;;;;

(in-package #:stumpwmrc)

(defparameter *current-keyboard-layout* nil)
(defparameter *list-of-keyboard-layout* nil)

(defun set-keyboard-layout (layout)
  (check-type layout keyword)
  (run-shell-command (strcat "setxkbmap " (string-downcase layout)))
  (setf *current-keyboard-layout* layout)
  (message "Switch to ~A keyboard layout" *current-keyboard-layout*))

(defmacro define-keyboard-layout (layout)
  (check-type layout keyword)
  `(progn
     (defcommand ,(symcat layout '-keyboard) () ()
       (set-keyboard-layout ,layout))
     (push ,layout *list-of-keyboard-layout*)))

(defcommand switch-keyboard-layout () ()
  (let* ((member (member *current-keyboard-layout* *list-of-keyboard-layout*))
	 (len (length member))
	 (layout (cond
		   ((or 
		     (null *current-keyboard-layout*)
		     (= len 1))
		    (first *list-of-keyboard-layout*))
		   ((>= len 2)
		    (second member)))))
    #+dev-test (list len member layout)
    (set-keyboard-layout layout)))

(define-keyboard-layout :us)
(define-keyboard-layout :ca)

