#|

WIP: barely started working on this: re-writting the `defapplication' macro using more clos, more validation, more tests, etc.

|#

(defun missing-slot (name)
  (error "The slot ~s must be supplied" name))

;; TODO defclass application
(defclass application ()
  ((name :initarg :name
         :initform (missing-slot "name")
         :accessor name)
   (classes :initarg :classes
         :initform nil
         :accessor classes)
   (args :initarg :args
         :initform nil
         :accessor args)))


(defun without-earmuffs (string-designator)
  (let* ((string (string string-designator))
         (length (length string)))
    (let ((leftp (char= #\* (char string 0)))
          (rightp (char= #\* (char string (1- length)))))
      (if (or leftp rightp)
          (subseq string
                  (if leftp 1 0)
                  (if rightp (1- length) length))
          string))))

(defmacro define-application (name &body specifications)
  (multiple-value-bind (application-initargs command-initargs)
      (parse-body specifications)
    (break "~s" application-initargs command-initargs)
    `(progn
       (defparameter ,name
         ,(apply #'make-instance 'application
                   :name name application-initargs)))))

(defmethod command ((app application))
  ())
