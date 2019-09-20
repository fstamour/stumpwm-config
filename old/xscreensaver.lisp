
(in-package #:stumpwmrc)

;; TODO if loaded "deamons"
(start-daemon :xscreensaver "xscreensaver")

(defcommand lock () ()
  ;;(run-shell-command "slock")
  (run-shell-command "xscreensaver-command --lock"))

(defcommand config-xscreensaver () ()
  (run-shell-command "xscreensaver-command --prefs"))

(defcommand activate-xscreensaver () ()
  (run-shell-command "xscreensaver-command --activate"))


