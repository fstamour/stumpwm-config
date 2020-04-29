;; -*-lisp-*-

;; http://www.emacswiki.org/emacs/StumpWM

(in-package #:stumpwmrc)

(defvar *swank-p* nil
  "Flag if swank is started or not")

(ignore-errors
  (load "~/.config/stumpwm/slime/swank-loader.lisp")
  (uiop:symbol-call :swank-loader :init)
  (defcommand swank
      (port) ((:number "Port: "))
      "Starts a swank server on port 4005 and notifies the user."
      (if *swank-p*
	  (message "Swank server already running.")
	  (progn
	    (uiop:symbol-call
	     :swank :create-server
	     :port port
	     ;; :style swank:*communication-style*
	     :dont-close t)
	    (setf *swank-p* t)
	    (echo-string
	     (current-screen)
	     "Starting swank. M-x slime-connect RET RET")))))

