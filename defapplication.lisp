;; 2022-11-11 I noticed the macro defprogram-shortcut was added, I
;; might be able to replace this macro with something built-in.
;;
;; 2024-10-05 Actually, no, I can't replace it because I'm using
;; "my-run-or-raise"

(in-package #:stumpwmrc)

;; TODO defclass application

(defun defapplication/docstring (name type)
  "Generate the docstring for the command."
  (format nil
          (ecase type
            (run-or-raise "Start ~(~a~) or switch to it if already running.")
            (new "Starts a new instance of ~a.")
            (windowlist "Select a window for a list of ~(~a~) windows."))
          name))

;; TODO defapplication/run (for use at runtime)

(defun defapplication/command (name type #| TODO &key run-command |# class args other-args)
  "
other-args are passed to run-or-raise
      args are passed to the executable
"
  (let ((class (or class (string-capitalize name))))
    (ecase type
      (run-or-raise
       `(funcall
         ;; in case 'my-run-or-raise can't be defined, which happened
         ;; to me once because it uses bordeaux-threads and it wasn't
         ;; loaded in the image
         (or (and (fboundp 'my-run-or-raise) 'my-run-or-raise)
                     'run-or-raise)
         ,(if args
              (downcase-cat name " " args)
              (downcase-cat name))
         ',(append other-args `(:class ,class))))
      (new `(run-shell-command ,(downcase-cat name)))
      ;; WARNING: this is my fork of stumpwm's windowlist
      (windowlist `(windowlist stumpwm::*window-format-by-class*
                               nil :filter (lambda (window)
                                             (and
                                              (not (eq window (current-window)))
                                              (string= (window-class window) ,class))))))))

#++
(list
 (defapplication/command 'firefox 'run-or-raise nil nil nil)
 (defapplication/command 'firefox 'new nil nil nil)
 (defapplication/command 'firefox 'run-or-raise "ff" nil nil)
 (defapplication/command 'firefox 'new "ff" nil nil)
 (defapplication/command 'firefox 'run-or-raise "ff" "-w" '("-a")))

;; TODO I think this could be a function
(defun defapplication/bindings (bind command)
  "Macro to create a binding for command"
  (when bind
    `(define-key ,(case (first bind)
                    (:top '*top-map*)
                    (:root '*root-map*)
                    (t (first bind)))
         (kbd ,(second bind))
       ,(format nil "~(~a~)" command))) )

#++
(list
 (defapplication/bindings '(:root "w") 'command)
 (defapplication/bindings '(:top "c") 'command)
 (defapplication/bindings '(*other-map* "c") 'command))

;; TODO It would be nice to check if the command already exists, try
;; to remove existing bindings
(defmacro defapplication (name
                          &rest rest
                          &key
                            args
                            bind
                            class
                            command-name
                            (type 'run-or-raise)
                            ;; TODO run-command
                            types
                          &allow-other-keys)
  "A macro to define common commands.

Let's you easily define, and optionally bind, a new stumpwm command with some common
behaviour, like \"run-or-raise\".

When type is 'NEW, the command will open a new instance of the
application even if one is already running. The stumpwm command will
have the \"-new\" suffix.

When TYPE is 'RUN-OR-RAISE, run-or-raise will be called to open the
application.

The parameter CLASS can be used to customize window
matching (e.g. when the command calls run-or-raise).

The BIND parameter, if specified, must be a list of two
elements (MAP-SPEC KEY). MAP-SPEC can be either :top, :root, or the
map itself.

Some examples:

(defapplication firefox :bind (:root \"w\"))
(defapplication firefox :type new t :bind (:root \"W\"))
(defapplication emacs :type new :bind (:root \"E\"))
(defapplication termite :class \"Termite\" :bind (:root \"c\"))"
  (check-type name symbol)
  (check-type type (member run-or-raise new windowlist))
  (when (and type types)
    (error "Only one of TYPE and TYPES can be specified at the same time"))
  ;; Some local variable
  (let ((other-args (remove-from-plist
                     rest :args :bind :class :command-name :type))
        (command (symcat (or command-name name) "-" type)))
    `(prog1
         ;; The command itself
         (defcommand ,command
             () ()
           ;; Docstring
           ,(defapplication/docstring name type)
           ;; The actual code
           ,(defapplication/command name type class args other-args))
       ;; Bindings
       ,(defapplication/bindings bind command))))

;; TODO find a better name ffs
(defun parse-body (specifications)
  (loop
    :for rest = specifications :then (cddr rest)
    :for (k v) := rest
    :while (keywordp k)
    :append (list k v) :into common
    :finally (return (values common rest))))

#++
(parse-body
 `((:bind (:root "e"))
   (:type new :bind (:root "E"))
   (:type windowlist :bind (:root "M-e"))))
;; => NIL,
;; ((:BIND (:ROOT "e")) (:TYPE NEW :BIND (:ROOT "E"))
;;                      (:TYPE WINDOWLIST :BIND (:ROOT "M-e")))

#++
(parse-body
 `(:class x
          (:bind (:root "e"))
          (:type new :bind (:root "E"))
          (:type windowlist :bind (:root "M-e"))) )
;; (:CLASS X)
;; ((:BIND (:ROOT "e")) (:TYPE NEW :BIND (:ROOT "E"))
;;                      (:TYPE WINDOWLIST :BIND (:ROOT "M-e")))

#++
(parse-body
 `(:class x
   :run-command "exec sh -c \"some weird command\""
          (:bind (:root "e"))
          (:type new :bind (:root "E"))
          (:type windowlist :bind (:root "M-e"))))
;; (:CLASS X :RUN-COMMAND "exec sh -c \"some weird command\"")
;; ((:BIND (:ROOT "e")) (:TYPE NEW :BIND (:ROOT "E"))
;;                      (:TYPE WINDOWLIST :BIND (:ROOT "M-e")))

;; WIP
(defmacro defapplication* (name &body specifications)
  (multiple-value-bind (common specifications)
      (parse-body specifications)
    `(progn
       ,@(loop :for spec :in specifications
               :collect `(defapplication ,name ,@common ,@spec)))))
