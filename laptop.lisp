
(in-package #:stumpwmrc)

(let ((script
       (merge-pathnames
	"nvidia-backlight-brightness.sh" *load-pathname*)))
  (defcommand brightness-up () ()
	      (run-shell-command (format nil "~a up" script)))
  (defcommand brightness-down () ()
	      (run-shell-command (format nil "~a down" script))))

(define-key *top-map* (kbd  "XF86MonBrightnessUp") "brightness-up")
(define-key *top-map* (kbd  "XF86MonBrightnessDown") "brightness-down")
 
