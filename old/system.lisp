
(in-package #:stumpwmrc) 

(defcommand suspend (args) ((:y-or-n "Are-you sure you want to suspend? "))
  (when (first args)
    (run-shell-command "systemctl suspend")))

(defcommand reboot (args) ((:y-or-n "Are-you sure you want to reboot? "))
  (when (first args)
    (run-shell-command "systemctl reboot")))

(defcommand shutdown (args) ((:y-or-n "Are-you sure you want to shutdown? "))
  (when (first args)
    (run-shell-command "systemctl poweroff")))

(defcommand lock-suspend (args) ((:y-or-n "Are-you sure you want to Lock and Suspend? "))
  (when (first args)
    (lock)
    (run-shell-command "systemctl suspend")))

