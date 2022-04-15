
(in-package #:stumpwmrc)

(defvar *battery-file*
  #-freebsd
  (probe-file "/sys/class/power_supply/BAT1/capacity")
  ;; I only have FreeBSD installed on my laptop
  #+freebsd
  t)

(defvar *last-battery-capacity* nil
  "On FreeBSD, the command sysctl take almost a second to complete. I
  don't want the modeline to take time to re-draw, so I'm caching the
  baterry capacity.")

#-freebsd
(defun battery-capacity ()
  (when *battery-file*
    (with-open-file (stream *battery-file*)
      (parse-integer
       (read-line stream)))))

#+freebsd
(defun battery-capacity ()
  (with-input-from-string (input (run-shell-command
                                  "sysctl hw.acpi.battery" t))
    (loop :for line = (read-line input nil nil)
          :while line
          :for battery-life-string =
                                   (multiple-value-bind
                                         (_ suffix)
                                       (alexandria:starts-with-subseq
                                        "hw.acpi.battery.life: "
                                        line
                                        :return-suffix t)
                                     (declare (ignore _))
                                     suffix)
          :when battery-life-string
            :do (return (parse-integer battery-life-string)))))


(defun battery-capacity-cached ()
  (when (or (null *last-battery-capacity*)
            (< 30
               (-
                (get-universal-time) (cdr *last-battery-capacity*))))
    (setf *last-battery-capacity*
          (cons (battery-capacity)
                (get-universal-time))))
  (car *last-battery-capacity*))

;; (battery-capacity)
;; (battery-capacity-cached)

;; Used to test changes to modeline's colors
(defun restart-mode-line-eveywhere ()
  (loop for screen in *screen-list* do
    (loop for head in (screen-heads screen) do
      (enable-mode-line screen head nil)
      (enable-mode-line screen head t))))

(defun enable-mode-line-eveywhere ()
  (loop for screen in *screen-list* do
    (loop for head in (screen-heads screen) do
      (enable-mode-line screen head t))))

;; The colors must be set before enabling the mode line
(setf *mode-line-foreground-color* "palegreen" ; Default is Gray50
      *mode-line-background-color* "Black")    ; Default is Gray20
(enable-mode-line-eveywhere)
;; (restart-mode-line-eveywhere)


;; *colors*
;; => ("black" "red" "green" "yellow" "blue" "magenta" "cyan" "white")
;;     ^0      ^1    ^2      ^3       ^4     ^5        ^6     ^7


;; Default *screen-mode-line-format*
;; => "[^B%n^b] %W"

(setf *screen-mode-line-format*
      `("[^B%g^b] [%v ] ^>"
        ;; Groups Windows
        ;; '(:eval (stumpwm:run-shell-command "date" t))
        ;; '(:eval (bar 50 10 #\x #\-))
        ,@(when *battery-file*
            '("[bat: "
              (:eval (bar (battery-capacity-cached) 10 #\x #\-))
              "] "))
        ;; (:eval (format nil "~a% " (battery-capacity)))
        "^5[^B%d^b]"))


;; Default *mode-line-highlight-template*
;; => "^R~A^r"
;; (setf *mode-line-highlight-template* "^B~a^b")


;; Default *window-format*
;; "%m%n%s%50t"

(setf *window-format*
      "^[^2%m%n^]^[^1%s^]%10c")
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
