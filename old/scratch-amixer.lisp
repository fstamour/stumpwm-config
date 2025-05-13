;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(in-package :stumpwm)

(defun def-volcontrol (channel amount)
      "Commands for controling the volume"
      (define-stumpwm-command
        (concat "amixer-" channel "-" (or amount "toggle")) ()
        (echo-string
         (current-screen)
         (concat channel " " (or amount "toggled") "
    "
                 (run-shell-command
                  (cat "amixer set "
		       channel " "
		       (or amount "toggle")
		       "| grep '^[ ]*Front'") t)))))



(defun volume-relative-set (amout &optional (channel "Master"))

(let* ((channel "Master")
       (get-out ))
  get-out)

(defparameter *get-out*
  (run-shell-command (stumpwm::cat "amixer get Master") t))

(split-sequence #\Space chan)

(key 

(defcommand volume-down () () 
  (run-shell-command "amixer set Master 500-"))
(defcommand volume-up () () 
  (run-shell-command "amixer set Master 500+"))

(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")


(define-keysym #x1008FF12 "XF86AudioMute")
(define-keysym #x1008FF13 "XF86AudioRaiseVolume")

;;; parse "amixer get <channel>"'s output
(defun get-volume (&optional (channel "Master"))
(with-collectors (vol channel)
		 (dolist (chan (butlast
				(last
				 (split-sequence #\Newline *get-out*)
				 3)))
		   (channel (first (split-sequence #\: chan)))
		   (vol (parse-integer
			(sixth (split-sequence #\Space chan)))

		    )))
(/ vol 655.36)  --> %

(use-package 'split-sequence)
(use-package 'cl-utilities)




