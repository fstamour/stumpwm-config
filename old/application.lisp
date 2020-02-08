;; -*-lisp-*-

(in-package #:stumpwmrc)

(require :alexandria)

(defvar *app-root-map* (make-sparse-keymap))
(define-key *top-map* (kbd "S-Menu") *app-root-map*)

(defmacro defapplication (name &rest rest &key args bind class command-name newp &allow-other-keys)
  "A macro to define commands."
  ;; Some local variable
  (let ((other-args (alexandria:remove-from-plist rest :args :bind :class :command-name :newp))
	(command (symcat (let ((basename (if command-name
					     command-name
					   name)))
			   (if newp
			       (symcat basename '-new)
			     basename)))))
    `(prog1
	 ;; The command itself
	 (defcommand ,command
	   () ()
	   ;; Docstring + Message
	   ,@(if newp
		 (list (cat "Start a new instance of " name ".")
		       `(message ,(format nil "Run a new instance of ~A" name)))

	       (list
		(cat "Start " (downcase-cat name)  " or switch to it if already running.")
		`(message ,(format nil "Run or raise ~A" name))))
	   ;; The run-shell-command OR run-or-raise.
	   ,(if newp
		`(run-shell-command ,(downcase-cat name)) ;; TODO Add Args here?
	      `(run-or-raise ,(if args
				  (downcase-cat name " " args)
				(downcase-cat name))
			     ',(append other-args
				       `(:class ,(if class
						     class
						   (string-capitalize name)))))))
       ;; Bindings
       ,(when bind
	  (let* ((listp (listp bind))
		 (map (if listp
			  (ecase (car bind)
			    (:top '*top-map*)
			    (:root '*root-map*)
			    (:app '*app-root-map*))
			'*app-root-map*))
		 (key (if listp
			  (second bind)
			bind)))
	    `(define-key ,map (kbd ,key) ,(cat command)))))))

(defmacro macro-for-each (macro over &body rest)
  `(,@(append (when over (list over))
	      (loop for el in rest collect
		    `(,macro ,@el)))))

(macro-for-each defapplication progn
  (arandr :bind "a")
  (chromium :bind "C")
  (conkeror :bind "c")
  (firefox :bind "f")
  (gmpc :command-name "mpd-interface" :bind "m")
  (gnome-terminal :class "terminal")
  (codeblocks)
  (emacs :bind "e")
  (emacs :newp t :bind "E")
  (freemind :class "FreeMind" :bind "b") ;; 'b' like in 'brainstorm'
  (finalterm)
  (nautilus :bind (:top "s-e"))
  (nautilus :newp t :bind (:top "s-E"))
  (poedit :bind "p")
  (shutter :bind (:top "Print"))
  (terminator :bind "t")
  (terminator :newp t :bind "T")
  (thunar)
  (thunar :newp t)
  (virtualbox :class "VirtualBox" :bind "V")
  (gvim :bind "v")
  (zim :bind "z"))


;; Choose the default terminal
(defcommand-alias terminal terminator)

;; Intellij
;; With some error handling
(defcommand intellij () ()
  "Start intellij or switch to it if already running."
  (let ((startup-script "~/bin/run-intellij"))
    (if (probe-file startup-script)
	(run-or-raise startup-script '(:class "jetbrains-idea"))
	(message "^B^1*The script \"^*~A^1\" required to start IntelliJ was not found."
		 startup-script))))

