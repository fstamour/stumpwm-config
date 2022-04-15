
(in-package #:stumpwmrc)

#-freebsd
(let ((script
        (merge-pathnames
         "nvidia-backlight-brightness.sh" *load-pathname*)))
  (defcommand brightness-up () ()
    (run-shell-command (format nil "~a up" script)))
  (defcommand brightness-down () ()
    (run-shell-command (format nil "~a down" script))))


#+freebsd
(defcommand brightness-up () ()
  (message "more light!")
  (run-shell-command "xbacklight -inc 10"))

#+freebsd
(defcommand brightness-down () ()
  (message "more darkness!")
  (run-shell-command "xbacklight -dec 10"))

(define-key *top-map* (kbd  "XF86MonBrightnessUp") "brightness-up")
(define-key *top-map* (kbd  "XF86MonBrightnessDown") "brightness-down")


;; 2021-11-21 On my laptop, FreeBSD does not recognize the "brightness
;; up/down" buttons.
;;
;; They don't show up in the window manager, nor in xev and not even
;; in (as root) libinput debug-events, which means the kernel doesn't
;; recognize the keys. I've read on a FreeBSD forum that ThinkPads
;; often uses ACPI for some keys. My laptop isn't a ThinkPad, but it's
;; a Lenovo (same company), they might be using the same "trick".
;; There's a "acpi_ibm.ko" kernel module, but I don't feel like trying
;; it right now...
