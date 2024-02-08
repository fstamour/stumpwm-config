(in-package #:stumpwmrc)

(defun emacs-focus ()
  ;; TODO this is a workaround the fact that my-run-or-raise (defined
  ;; in mru.lisp) is actually a "run, or raise, or raise the next", but
  ;; here I really just want "run or raise". I can't use the built-in
  ;; "run-or-raise" either, because it might raise another window even
  ;; if the current one matches.
  ;;
  ;; TODO defapplication could generate a predicate "emacs-current-window-p"
  (unless (window-class= (current-window) "Emacs")
    (emacs-run-or-raise)))



(defun print-string-for-emacs (stream string)
  (write-char #\" stream)
  (loop
    :with length = (length string)
    :for prev := nil :then c
    :for c :across string
    :for i :from 1
    :for lastp := (= i length)
    :for escp := (and prev (char= #\\ prev))
    :do
       (if escp
           (cond
             ((or (position c "rnNCu\"\\^" :test 'char=)
                  (char= #\" c))
              (write-char #\\ stream) (write-char c stream))
             (t (write-char #\\ stream) (write-char #\\ stream) (write-char c stream)))
           (when (char/= #\\ c)
             (write-char c stream)))
    :when (and lastp (char= c #\\))
      :do (error "Can't end a string with a single backslash!"))
  (write-char #\" stream))

(defun print-pathname-for-emacs (stream pathname)
  (write (namestring pathname) :stream stream))

(defun make-emacs-pprint-dispatch ()
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch *print-pprint-dispatch*)))
    (set-pprint-dispatch 'pathname 'print-pathname-for-emacs)
    (set-pprint-dispatch 'string 'print-string-for-emacs)
    *print-pprint-dispatch*))

(defun print-for-emacs (form)
  ;; TODO don't compute the dispatch table every time
  (let ((*print-pprint-dispatch* (make-emacs-pprint-dispatch))
        (*print-case* :downcase))
    (prin1-to-string form)))

(defun test-print-for-emacs (form expected-string)
  (let ((got (print-for-emacs form)))
    (values (string= got expected-string) got)))

#++
(list
 (test-print-for-emacs "\\n" "\"\\n\"")
 (test-print-for-emacs "\\C" "\"\\C\"")
 (test-print-for-emacs "\\c" "\"\\\\c\"")
 (not (ignore-errors (test-print-for-emacs "\\" 'signal)))
 (test-print-for-emacs "\\N{Infinity}" "\"\\N{Infinity}\"")
 (test-print-for-emacs "\\N{Infinity}" "\"\\N{Infinity}\"")
 (test-print-for-emacs '(foo "hi") "(foo \"hi\")")
 (let ((p (user-homedir-pathname)))
   (test-print-for-emacs p (prin1-to-string (namestring p)))))

(defun emacs-server-eval-at
    (form &key
            inhibit-message
            process-client-result
            run-program-args
            emacs-extra-args
            (split-output-p t)
            split-string-args
            focusp)
  "Evaluates a form in an emacs server. Returns the stdout stripped and
split. Signals an error if the emacs process exited with a non-zero
status.

FORM, PROCESS-RESULT and INHIBIT-MESSAGE are used to compute the form
sent to emacs.

RUN-PROGRAM-ARGS, EMACS-EXTRA-ARGS and SPLIT-STRING-ARGS controls how
the process is called and it's result transformed.

Tips for debugging:
 - don't inhibit-message, look in *Message* buffer
 - :run-program-args '(:ignore-error-status t) to see the stdout and stderr"
  (let* ((form (if process-client-result
                   form
                   #| When FORM evaluates to something that has no
printable representation, server-eval-at (in the emacs client) will
signal an error when trying to read the unreadable representation.

To avoid this, when the user doesn't need the results and only
evaluates something for the side-effects, we make sure the emacs
server to simply return the symbol t instead of whatever the form
would have evaluated to. |#
                   `(progn ,form t)))
         (form (if inhibit-message
                   `(let ((inhibit-message t)) ,form)
                   form))
         (form `(server-eval-at "server" ',form))
         (form (etypecase process-client-result
                 (null form)
                 (symbol `(,process-client-result ,form))
                 (cons `(,@process-client-result ,form))
                 (function (funcall process-client-result form))))
         (form `(progn (require 'server) ,form))
         (form-string (print-for-emacs form)))
    ;; (format *debug-io* "~&form: ~s~%form-string: ~s~%" form form-string)
    (multiple-value-bind (stdout stderr exit-status)
        (apply #'uiop:run-program
               `("emacs" "--batch" ,@emacs-extra-args "--eval" ,form-string)
               :error '(:string :stripped t)
               :output '(:string :stripped t)
               run-program-args)
      (let ((out (if (alexandria:emptyp stdout) stderr stdout)))
        (prog1
            (if (and split-output-p (zerop exit-status))
                (apply #'split-string* out split-string-args)
                out)
          (when focusp (emacs-focus)))))))

#++
(emacs-server-eval-at '(emacs-pid) :split-output-p nil)



;; emacs-open-project is now covered by quicksearch, but I'm keeping
;; this as an example of "loading a in emacs' server because there's
;; too much code".
#++
(defcommand emacs-open-project () ()
  (emacs-server-eval-at
   `(progn
      (load ,(uiop:xdg-config-home "stumpwm" "emacs-open-project.el"))
      (fsta/switch-project ""))
   :inhibit-message t
   :focusp t))

(defun emacs-list-repositories ()
  (ignore-errors ; Future me will have to debug this
   (emacs-server-eval-at
    '(string-join (magit-list-repos) "\\n")
    :inhibit-message t
    :process-client-result 'princ)))

#++ (emacs-list-repositories)

(defun emacs-find-file (pathname)
  (emacs-server-eval-at
   `(find-file ,(truename pathname))
   :focusp t))

#++ (emacs-find-file (user-homedir-pathname))

(defun emacs-magit (pathname)
  (emacs-server-eval-at
   `(magit-status ,(truename pathname))
   :focusp t))

#++
(emacs-magit (merge-pathnames "dev/lem" (user-homedir-pathname)))
