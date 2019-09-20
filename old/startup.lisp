
;;;
;;; Things to do at startup!
;;; 

(in-package #:stumpwmrc)

(defparameter *starup-process* (make-hash-table)
  "Not garanteed that it contains something useful, but under SBCL, you have a structure of type SB-IMPL::PROCESS which can be used.")

(start-daemon :dropbox "dropbox start" *starup-process*)
(start-daemon :cl-sulfur "cd ~/quicklisp/local-projects/cl-sulfur && make start-in-screen" *starup-process*)

#+ (or) (dump-daemons *starup-process*)

