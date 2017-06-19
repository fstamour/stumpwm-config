
;;; Some string munching utilities
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
