(in-package #:stumpwm-user)

;; Change the color of the border around the focused window (default is white)
;; (set-focus-color "orange")
(set-focus-color "black")

(setf *message-window-gravity* :center
      *message-window-padding* 15
      *message-window-y-padding* 15)

(setf *input-window-gravity* :center)
