
(in-package #:stumpwmrc)

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
      :keyword))))

(try-load "application.lisp")
(try-load "daemons.lisp")
(try-load "locale.lisp")
(try-load "xscreensaver.lisp")
(try-load "contrib.lisp")
(try-load "modeline.lisp")
(try-load "swank-loader.lisp")
(try-load "swank.lisp")
(try-load "system.lisp")
(try-load "ssh-agent.lisp")
(try-load "mpd.lisp")
(try-load "hotkeys.lisp")
(try-load "notes.lisp")
(try-load "bindings.lisp")
(try-load "stump-tweaks.lisp")
(try-load "startup.lisp") ;; dropbox, cl-sulfur.
(try-load "xrandr.lisp")

#+ (or) (progn
	  ;; Create a new group
	  (run-commands "gnew system")
	  ;; Run Trayer
	  (run-shell-command "exec /usr/bin/trayer --SetDockType false")
	  ;; Wait for Trayer
	  (sleep 1)
	  ;; Go back to "Main" group.
	  (run-commands "gselect 1"))

;; Change the Stumpwm prefix key: Use the menu key
(set-prefix-key (kbd "Menu"))

;; Set the background
(run-shell-command "display -window root ~/Backgrounds/metal-hole2.jpg")

;; Some minimal keybindings
(define-key *top-map* (kbd "M-TAB") "fnext")
(define-key *top-map* (kbd "M-`") "pull-hidden-previous")
(define-key *top-map* (kbd "M-~") "pull-hidden-next")

