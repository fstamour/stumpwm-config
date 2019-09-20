
(in-package #:stumpwmrc)

(defcommand volume-down () () 
  (run-shell-command "amixer set Master 500-"))

(defcommand volume-up () () 
  (run-shell-command "amixer set Master 500+"))

(defcommand mute-toggle () () 
  (run-shell-command "amixer set Master toggle"))
