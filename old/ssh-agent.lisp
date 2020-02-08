
(in-package #:stumpwmrc)

(defparameter *ssh-agent-output*
  (run-shell-command "ssh-agent" T))

(defcommand ssh-add () ()
    (run-shell-command "ssh-add"))

#+dev-test (collecting
	     (dolist (line (split-sequence #\Newline *ssh-agent-output*))
	       (collect (split-sequence #\; line :count 1))))

;; (getenv "SSH_AGENT_PID")
;; SSH_ASKPASS=/usr/lib/ssh/x11-ssh-askpass
;; (getenv "DISPLAY")
;; (getenv "SSH_ASKPASS")





