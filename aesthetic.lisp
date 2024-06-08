(in-package #:stumpwm-user)

;; https://stumpwm.github.io/git/stumpwm-git_38.html#Customizing-Window-Appearance

(setf *window-border-style* :none)

;; Change the color of the border around the focused window (default is white)
(set-focus-color "orange")

(setf *message-window-gravity* :center
      *message-window-padding* 15
      *message-window-y-padding* 15)

(setf *input-window-gravity* :center)
