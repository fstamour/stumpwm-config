
(in-package #:stumpwmrc)

(defun toggle-mode-line-eveywhere ()
  (loop for screen in *screen-list* do
    (loop for head in (screen-heads (current-screen)) do
	    (toggle-mode-line screen head))))


;; Default *screen-mode-line-format*
;; => "[^B%n^b] %W"

(setf *screen-mode-line-format*
      "[^B%d^b] [^B%g^b] [%W ]")
;;      Time    Groups   Windows

;; Default *window-format*
;; "%m%n%s%50t"

(setf *window-format*
      "%m%n%s%10c")
#|
%m Draw a # if the window is marked.
%n The window's number
%s The window's status.
  * means current window,
  + means last, and
  - means any other window
%10c The windows' class, cropped to 10 characters
|#

(toggle-mode-line-eveywhere)

;; Default is 60.
(setf *mode-line-timeout* 1)
(stumpwm::turn-on-mode-line-timer)

