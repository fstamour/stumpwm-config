;; -*-lisp-*-

;; http://www.emacswiki.org/emacs/StumpWM

(in-package #:stumpwmrc)

(merge-pathnames "slime/swank-loader.lisp" *stumpwmrc-directory*)
(swank-loader:init)
(defvar *swank-p* nil)

(defcommand swank (port) ((:number "Port: "))
  "Starts a swank server on port 4005 and notifies the user."
  (if *swank-p*
      (message "Swank server already running.")
      (progn
	(swank:create-server :port port
			     :style swank:*communication-style*
			     :dont-close t)
	(setf *swank-p* t)
	(echo-string
	 (current-screen)
	 "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm)"))))

