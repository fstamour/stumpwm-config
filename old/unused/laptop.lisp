
(in-package :stumpwm)


(defparameter *test-out* (run-shell-command "acpi" t))


(defun hms->s (hour min sec)
  (+
   (* 3600 hour)
   (* 60 min)
   sec))

(defun s->hms (sec)
  (multiple-value-bind (hour remainder)
      (floor 
       (/ sec 3600.0))
    (multiple-value-bind (minute remainder2)
    (floor
     (/ (* 3600 remainder)
	60))
  (values hour minute (* remainder2 60)))))

;; (s->hms 3782)

(ql:quickload 'split-sequence)

;; This code is "valid", but doesn't check if it's "charging" or "uncharging"
(s->hms
 (let ((acpi-output (run-shell-command "acpi" t)))
   (apply 'hms->s
	  (mapcar 'parse-integer
		  (split-sequence:split-sequence
		   #\:
		   (fifth
		    (split-sequence:split-sequence #\Space acpi-output)))))))

(defun slurp-stream (stream)
  (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq stream))
    seq))

(defun slurp-file (filespec)
  (with-open-file (input filespec)
    (slurp-stream input)))

(parse-integer (slurp-file "/sys/class/power_supply/BAT1/capacity"))

(slurp-file "/sys/class/power_supply/BAT1/status")

;; TODO: screen brightness
