
(in-package #:stumpwmrc)

(defun defapplication/docstring+message (name newp)
  (if newp
      (list (cat "Starts a new instance of " name ".")
            `(message ,(format nil "Run a new instance of ~A" name)))

      (list
       (cat "Start " (downcase-cat name)  " or switch to it if already running.")
       `(message ,(format nil "Run or raise ~A" name)))))


#+nil
(progn
  (defapplication/docstring+message 'firefox nil)
  (defapplication/docstring+message 'firefox t))

(defun defapplication/command (name newp class args other-args)
  "
other-args are passed to run-or-raise
      args are passed to the executable
"
  (if newp
      `(run-shell-command ,(downcase-cat name)) ;; TODO Add Args here?
      `(run-or-raise
	,(if args
             (downcase-cat name " " args)
             (downcase-cat name))
        ',(append other-args
                  `(:class ,(if class
                                class
                                (string-capitalize name)))))))

#+nil
(progn
  (defapplication/command 'firefox nil nil nil nil)
  (defapplication/command 'firefox t nil nil nil)
  (defapplication/command 'firefox nil "ff" nil nil)
  (defapplication/command 'firefox t "ff" nil nil)
  (defapplication/command 'firefox nil "ff" "-w" '("-a")))

(defun defapplication/bindings (bind command)
  (when bind
    (let* ((listp (listp bind))
           (map (if listp
                    (ecase (car bind)
                      (:top '*top-map*)
                      (:root '*root-map*)
                      (:app '*app-root-map*))
                    '*app-root-map*))
           (key (if listp (second bind) bind)))
      `(define-key ,map (kbd ,key) ,(format nil "~(~a~)" command)))) )

#+nil
(progn
  (defapplication/bindings '(:root "w") 'command)
  (defapplication/bindings '(:top "c") 'command)
  (defapplication/bindings '(:app "c") 'command)
  (defapplication/bindings "c" 'command))

(defmacro defapplication (name
			  &rest rest
			  &key args bind class command-name newp
			    &allow-other-keys)
  "A macro to define commands.
Let's you easily define a new stumpwm command with some common behaviour, like \"run-or-raise\" and then bind the command

If the \"newp\" parameter is true, the command will show a message first saying that it's opening a new instance of the application even if one is already running. The stumpwm command will have the \"-new\" suffix.

If the \"newp\" parameter is false \"run-or-raise\" will be called to open the application. The parameter \"class\" can be used to tweak the \"raise\" part.

The \"bind\" parameter can be either a key or a list.
If it's a key, the binding will be added to the *app-root-map* map.
If it's a list, it is expected to have the form (map-spec key)
The \"map-spec\" can be either :top, :root, :app or the map itself.

Some examples:

(defapplication firefox :bind (:root \"w\"))
(defapplication firefox :newp t :bind (:root \"W\"))
(defapplication emacs :newp t :bind (:root \"E\"))
(defapplication termite :class \"Termite\" :bind (:root \"c\"))"
  (check-type name symbol)
  ;; Some local variable
  (let ((other-args (remove-from-plist
		     rest :args :bind :class :command-name :newp))
        (command (symcat (or command-name name) (if newp '-new ""))))
    `(prog1
         ;; The command itself
         (defcommand ,command
             () ()
           ;; Docstring + Message
	   ,@(defapplication/docstring+message name newp)

           ;; The run-shell-command OR run-or-raise.
	   ,(defapplication/command name newp class args other-args))
       ;; Bindings
       ,(defapplication/bindings bind command))))
