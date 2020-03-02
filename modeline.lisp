
(in-package #:stumpwmrc)

(defun enable-mode-line-eveywhere ()
  (loop for screen in *screen-list* do
    (loop for head in (screen-heads (current-screen)) do
	    (enable-mode-line screen head t))))

(enable-mode-line-eveywhere)

;; *colors*
;; => ("black" "red" "green" "yellow" "blue" "magenta" "cyan" "white")
;;     0       1     2       3        4      5         6      7


;; Default *screen-mode-line-format*
;; => "[^B%n^b] %W"

(setf *screen-mode-line-format*
      `("[^B%g^b] [%W ] ^>"
;;           Groups Windows
       ;;  '(:eval (stumpwm:run-shell-command "date" t))
        ;; (:eval (bar 50 10 #\x #\-))
        "^5[^B%d^b]"))


;; Default *window-format*
;; "%m%n%s%50t"

(setf *window-format*
      "^2%m%n^n^1%s^n%10c")
#|
%m Draw a # if the window is marked.
%n The window's number
%s The window's status.
  * means current window,
  + means last, and
  - means any other window
%10c The windows' class, cropped to 10 characters
|#


;; Default is 60.
(setf *mode-line-timeout* 1)
(stumpwm::turn-on-mode-line-timer)

