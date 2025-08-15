
(in-package #:stumpwmrc)

;; Screen Lock
(defcommand lock () ()
  (run-commands "exec xlock"))

;; Suspend
(defcommand suspend (confirmed-p)
  ((:y-or-n "About to suspend (without locking). U sure? "))
  (when confirmed-p
    (if (systemctlp)
        (run-commands "exec systemctl suspend")
        (run-commands "exec loginctl suspend"))))

;; Lock & Suspend
(defcommand lock-suspend (confirmed-p)
  ((:y-or-n "About to suspend (and lock). U sure? "))
  (when confirmed-p
    (run-commands
     "lock"
     (if (systemctlp)
         "exec systemctl suspend"
         "exec loginctl suspend"))))

;; Shutdown
(defcommand shutdown (confirmed-p)
    ((:y-or-n "About to shutdown. U sure? "))
  (when confirmed-p
    (run-commands
     (if (systemctlp)
         "exec systemctl poweroff"
         "exec loginctl poweroff"))))

(defcommand reboot (confirmed-p)
    ((:y-or-n "About to reboot. U sure? "))
  (when confirmed-p
    (run-commands
     (if (systemctlp)
         "exec systemctl reboot"
         "exec loginctl reboot"))))

;; at this point... should I just use loginctl all the time?
