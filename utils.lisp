
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

(defun xdg-open (thing)
  (uiop:launch-program `("xdg-open" ,thing)
                       ;; :input nil
                       ;; :output *query-io* ;; '(:string :stripped t)
                       ;; :ignore-error-status t
                       ))



(defgeneric window-class= (a b)
  (:method ((w window) (class string))
    (string= (window-class w) class))
  (:method ((class string) (w window))
    (string= (window-class w) class))
  (:method ((w1 window) (w2 window))
    (string= (window-class w1) (window-class w2))))



(defun split-string* (string &key max (separator '(#\Newline)))
  "Same as uiop:split-string, but split by newlines by default instead of
spaces and tabs."
  (uiop:split-string
   string
   :max max
   :separator (or separator '(#\Newline))))



(defun map-lines (path callback)
  (alexandria:with-input-from-file (in path)
    (loop
      :for line = (read-line in nil nil)
      :while line
      :unless (or (alexandria:emptyp line)
                  (char= #\# (char line 0)))
        :do (funcall callback line))))

(defmacro with-reading-lines ((path line) &body body)
  `(map-lines
    ,path
    (lambda (,line) ,@body)))

(defun map-tsv (path callback)
  (with-reading-lines (path line)
    (funcall callback (uiop:split-string line :separator '(#\Tab)))))

(defmacro with-tsv ((path cells) &body body)
  `(map-tsv ,path
            (lambda (,cells)
              ,@body)))

(defmacro with-hash-table ((&optional destination) &body body)
  (alexandria:with-gensyms (map)
    `(let ((,map (or ,destination (make-hash-table :test 'equal))))
       (flet
           ;; TODO gensym fetch
           ((fetch (k) (gethash k ,map))
            ;; TODO gensym add
            (add (k v)
              (setf (gethash k ,map) v)))
         (declare (ignorable #'fetch #'add))
         ,@body
         ,map))))

#++
(with-hash-table ()
  (add :k 'value))
