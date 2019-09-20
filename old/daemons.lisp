
(in-package #:stumpwmrc)

(defparameter *daemons* (make-hash-table)
  "Not garanteed that it contains something useful, but under SBCL, you have a structure of type SB-IMPL::PROCESS which can be used.")

(defun start-daemon (name cmd &optional collect-output-p (daemon-hashtable *daemons*))
  "See notes on *daemons*."
  (let ((process (run-shell-command cmd)))
    (log-message :info "Daemon \"~A\" started with command \"~A\"." name cmd)
    (setf (gethash name daemon-hashtable) process)))

(defun dump-daemons (&optional (daemon-hashtable *daemons*))
  (loop for value being the hash-values of daemon-hashtable
     using (hash-key key)
     collect (list key value)))

