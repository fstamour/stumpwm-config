;; -*-lisp-*-

;; http://www.emacswiki.org/emacs/StumpWM

(in-package #:stumpwmrc)

(defvar *swank-p* nil
  "Flag if swank is started or not")

(defcommand swank
    (port) ((:number "Port: "))
  "Starts a swank server on port 4005 and notifies the user."
  (if (not (find-package 'swank))
      (message "Swank not loaded...")
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
	     "Swank started M-x slime-connect RET RET")))))


(unless (or
	 ;; already started
	 *swank-p*
	 (and
	  ;; already loaded
	  (find-package "swank")
	  ;; already started, but not with the "swank" command
	  (uiop:symbol-call :swank :connection-info)))

  ;; Try to load swank using swank-loader (make senses, right?)
  (ignore-errors
   (load "~/.config/stumpwm/slime/swank-loader.lisp")
   (uiop:symbol-call :swank-loader :init))

  ;; Second try...
  (ignore-errors
   (unless
       (load "~/.config/stumpwm/slime/swank.asd")
     (require 'swank))))
