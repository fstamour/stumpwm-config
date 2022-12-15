;; -*-lisp-*-
;;;;
;;;; http://www.emacswiki.org/emacs/StumpWM
;;;;
;;;; I can't load swank with stumpwm from nix, because sbcl-cltl2 is
;;;; required but it's not included...
;;;;

(in-package #:stumpwmrc)

(defparameter +slime-submodule+ "~/.config/stumpwm/slime/")

(defvar *swank-p* nil
  "Flag if swank is started or not")

(defun swank-available-p ()
  "Test if the swank package is available"
  ;; already loaded
  (find-package '#:swank))

(defun swank-started-p ()
  ""
  (and
   ;; loaded but not started
   (swank-available-p)
   (or
    ;; already started
    *swank-p*
    ;; already started, but not with the "swank" command
    (ignore-errors
      (uiop:symbol-call :swank :connection-info)))))

(defun load-swank-from (slime-root)
  (unless (swank-started-p)
    ;; TODO Check if slime-root exists
    (let ((swank-loader (merge-pathnames "swank-loader.lisp" slime-root))
          (swank-asd (merge-pathnames "swank.asd" slime-root)))

      ;; Try to load swank using swank-loader
      (ignore-errors
        (load swank-loader)
        (uiop:symbol-call :swank-loader :init :reload t :load-contribs t :quiet nil)
        (message "Loaded ~s successfully." swank-loader)
        (return-from load-swank-from))
      (message "Failed to load ~s." swank-loader)

      ;; Load the asd file if it swank-loader didn't work for some reason
      (ignore-errors
        (if (asdf:load-asd swank-asd)
            (progn
              (asdf:load-system "swank")
              (message "Loaded ~s successfully." swank-asd)
              (return-from load-swank-from))
            (message "Failed to load system definition ~s." swank-asd)))
      (message "Failed to load swank system." swank-asd))))

(unless (probe-file +slime-submodule+)
  (message "~&The folder \"~a\" doesn't exists, have you clone the submodule?"
           +slime-submodule+))

(defcommand load-swank () ()
  (load-swank-from +slime-submodule+))

(defun %swank-create-server (port)
  (uiop:symbol-call
   :swank :create-server
   :port port
   ;; :style swank:*communication-style*
   :dont-close t)
  (setf *swank-p* t)
  (echo-string
   (current-screen)
   "Swank started M-x slime-connect RET RET"))

(defcommand swank-create-server
    (port) ((:number "Port: "))
  "Starts a swank server on port 4005 and notifies the user."
  (if (swank-started-p)
      (message "Swank server already running.")
      (%swank-create-server port)))

(defcommand swank
    () ()
  "Starts a swank server on port 4005 and notifies the user."
  (if (not (swank-available-p))
      (message "Swank not loaded, try the ~a command." #'load-swank)
      (run-commands "swank-create-server")))
