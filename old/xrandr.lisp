
(in-package #:stumpwmrc)

(defvar *hdmi-on-p* nil
  "Variable to keep track of whether the display on hdmi-1 is on or off.")

(defcommand enable-hdmi () ()
  "Call xrandr to turn on the hdmi display."
  (run-shell-command "xrandr --output HDMI-1 --mode 1920x1080 --pos 3600x0"))

(defcommand disable-hdmi () ()
  "Call xrandr to turn off the hdmi display."
  (run-shell-command "xrandr --output HDMI-1 --off"))

(defcommand toggle-hdmi (args) ((:y-or-n "Are-you sure you want to toggle HDMI display? "))
  "Toggle the hdmi display"
  (when (first args)
    (if *hdmi-on-p*
	(disable-hdmi)
	(enable-hdmi))
    (setf *hdmi-on-p* (not *hdmi-on-p*))))
