
(in-package #:stumpwmrc)

;;; Some string munching utilities
;;; TODO I'm not even sure if I still use these
(eval-when (:execute :load-toplevel :compile-toplevel)
  (export
   (defun cat (&rest rest)
     "Concatenate all its argument into a string."
     (format nil "~{~A~}" rest)))

  (export
   (defmacro cat* (&rest rest)
     "A macro that concatenate all its argument into a string."
     (format nil "~{~A~}" rest)))

  (export
   (defmacro strcat (&rest rest)
     `(concatenate 'string ,@rest)))

  (export
   (defun upcase-cat (&rest rest)
     "Concatenate all its upcased argument into a string."
     (format nil "~@:(~{~A~}~)" rest)))

  (export
   (defun downcase-cat (&rest rest)
     "Concatenate all its downcased argument into a string."
     (format nil "~(~{~A~}~)" rest)))

  (export
   (defun space-cat (&rest rest)
     "Like cat, but with space between element.
I didn't really need it to be a function, but I coulnd't help myself with a
function name like that."
     (format nil "~{~A~^ ~}" rest)))

  (export
   (defun symcat (&rest rest)
     "Concatenate all its argument into a string. Returns a symbol created from that string. THE ARGUMENT ARE UPCASED."
     (intern
      (format nil "~@:(~{~A~}~)" rest))))

  (export
   (defun kwcat (&rest rest)
     "Concatenate all its argument into a string. Returns a keyword created from that string. THE ARGUMENT ARE UPCASED."
     (intern
      (format nil "~@:(~{~A~}~)" rest)
      :keyword)))

  ;; Same algo as the one from alexandria
  (defun remove-from-plist (plist &rest keys)
    (loop :for (key value) :on plist :by #'cddr
          :unless (member key keys :test #'eq )
            :collect key and collect (first rest))))



(defun laptop-p ()
  (string= "phi" (cl:machine-instance)))

(defun guixp ()
  #-freebsd
  (ignore-errors
   (search "Guix"
           (uiop:read-file-line "/etc/os-release"))))

(defun systemctlp ()
  (not (guixp)))



(defparameter *last-sh* nil
  "For debugging purpposes only: keep information about the last time the function sh was called.
Trace could be useful too (especially that this only keeps the last invocation.0")

(defun sh (control-string &rest format-arguments)
  "Run a command in a shell, wait for it, return its output as a list of lines (strings)."
  (setf *last-sh* (list control-string format-arguments)) ; for debugging
  (let* ((command (apply #' format nil control-string format-arguments))
         (result (split-string (run-shell-command command t))))
    (prog1 result
      ;; More bebugging stuff
      (setf (cdr (last *last-sh*))
            (list command result)))))
