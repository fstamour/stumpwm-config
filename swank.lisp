;; -*-lisp-*-

;; http://www.emacswiki.org/emacs/StumpWM

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

    ;; Try to load swank using swank-loader
    (ignore-errors
     (with-restarts-menu
         (load (merge-pathnames "swank-loader.lisp" slime-root))
       (uiop:symbol-call :swank-loader :init)
       (return-from load-swank-from)))

    ;; Load the asd file if it swank-loader didn't work for some reason
    (ignore-errors
     (with-restarts-menu
         (unless (asdf:load-asd (merge-pathnames "swank.asd" slime-root))
           (asdf:load-system '#:swank))))))

(unless (probe-file +slime-submodule+)
  (message "~&The folder \"~a\" doesn't exists, have you clone the submodule?"
           +slime-submodule+))

(defcommand load-swank () ()
  (load-swank-from +slime-submodule+))


(defun swank-create-server (port)
  (uiop:symbol-call
   :swank :create-server
   :port port
   ;; :style swank:*communication-style*
   :dont-close t)
  (setf *swank-p* t)
  (echo-string
   (current-screen)
   "Swank started M-x slime-connect RET RET"))

(defcommand swank
    (port) ((:number "Port: "))
  "Starts a swank server on port 4005 and notifies the user."
  (if (not (swank-available-p))
      (message "Swank not loaded, try the ~a command." #'load-swank)
      (if (swank-started-p)
          (message "Swank server already running.")
          (swank-create-server port))))
