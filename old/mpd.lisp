
(in-package #:stumpwmrc)

(start-daemon :mpd "mpd")

(defmacro define-simple-mpc-command (name)
  `(defcommand ,(symcat "music-" name) () ()
     (run-shell-command ,(downcase-cat "mpc " name))))

(defmacro define-simple-mpc-command* (&body body)
  `(progn
     ,@ (loop for name in body collect
	     `(define-simple-mpc-command ,name))))

(define-simple-mpc-command*
  play toggle next prev stop)


;;;; Code that use the module "mpd"
;;;; I don't use it, the last time I tried it failed miserably.
;; (load-module "mpd")

;; (in-package #:mpd)

;; (mpd-connect)
;; (mpd-disconnect)

;; (mpd-browse-playlist)
;; (mpd-status)
