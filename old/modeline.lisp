;; -*-lisp-*-

(in-package #:stumpwmrc)

;;(message "Loading rc!")


;; Tweak mode line
(setq *mode-line-pad-y* 0)
(setq *mode-line-border-width 0)
	   
;; Turn on mode line.
(loop for screen in *screen-list* do
     (loop for head in (stumpwm::screen-heads (current-screen)) do
	  (toggle-mode-line screen head)))
;; ^^ @todo stumpwm should export screen-heads

#+(or) (setf *screen-mode-line-format* 
	     " %l [^B%n^b] %W")

(in-package #:stumpwm)

;; @todo I wish I could do that without adding the function in stumpwm's package.
(defun my-fmt-head-window-list-hidden-windows (ml)
  "Using *window-format*, return a 1 line list of the windows, space
separated. The currently focused window is highlighted with
fmt-highlight. Any non-visible windows are colored the
*hidden-window-color*."
  (let* ((all (head-windows (mode-line-current-group ml) (mode-line-head ml)))
         (non-top (set-difference all (top-windows))))
    (format nil "~{~a~^ ~}"
            (mapcar (lambda (w)
                      (let ((str (format-expand *window-formatters*
                                                "%n%s%c" w)))
                        (cond ((eq w (current-window)) (fmt-highlight str))
                              ((find w non-top) (fmt-hidden str))
                              (t str))))
                    (sort1 all #'< :key #'window-number)))))

(push '(#\m my-fmt-head-window-list-hidden-windows)
      *screen-mode-line-formatters*)

(in-package #:stumpwm)



(setf *screen-mode-line-format* "%d %m")

;; mpd
;; (setf *screen-mode-line-format* "%m")

;; Default is 60.
(setf *mode-line-timeout* 15)
(stumpwm::turn-on-mode-line-timer)
