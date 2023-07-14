
(in-package #:stumpwmrc)

;; sanitiy check:
;; (parse-integer "" :junk-allowed t) ; => nil
;; (parse-integer nil :junk-allowed t) ; => error

;; TODO check if playerctl, amixer, grep
;; TODO *alsa-device* and *alsa-control*

(defun get-volume ()
  (let* ((device "pulse")
         (control "Master")
         ;; command-output is a list of lines, the grep keeps only the
         ;; percentages. For example, if you have 2 "outputs" Left and
         ;; Right, both at 40% the list will be '(40 40).
         (command-output
           (sh
            "amixer --device ~a  get ~a | grep -Po \"[0-9]+(?=%)\""
            device
            control))
         (volume-string (first command-output)))
    (when volume-string
      (parse-integer volume-string :junk-allowed t))))

#++ (run-shell-command "amixer --device pulse get Master" t)

(defun set-volume (volume)
  ;; TODO validate volume
  ;; TODO pass device and control
  ;; TODO support jack?
  (let* ((device "pulse")
         (control "Master"))
    (sh "amixer --device ~a sset ~a ~d" device control volume)
    (get-volume)))

(defun show-volume (&optional volume)
  (let ((volume (or volume (get-volume))))
    (if volume
        (sh "volnoti-show ~d" volume)
        (message "Failed to get the current volume"))))

(defcommand volume-up (&optional (step 2)) ()
  (set-volume (format nil "~d%+" step))
  (show-volume))

(defcommand volume-down (&optional (step 2)) ()
  (set-volume (format nil "~d%-" step))
  (show-volume))

#++
(progn
  (get-volume)
  (set-volume "30%")
  (show-volume)
  (volume-up)
  (volume-down))

#++ TODO
(defcommand mute-toggle () ()
  (run-shell-command "amixer set Master toggle"))

;; for testing: (mute-toggle)

(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")
#++ (define-key *top-map* (kbd "XF86AudioMute") "mute-toggle")

(defcommand music-toggle () ()
  (run-shell-command "playerctl play-pause"))

(defcommand music-next () ()
  (run-shell-command "playerctl next"))

(defcommand music-prev () ()
  (run-shell-command "playerctl prev"))

;;
;; (define-key *top-map* (kbd "XF86AudioStop") "music-stop")

(define-key *top-map* (kbd "XF86AudioPlay") "music-toggle")
(define-key *top-map* (kbd "XF86AudioNext") "music-next")
(define-key *top-map* (kbd "XF86AudioPrev") "music-prev")

#++
(progn
  (undefine-key *top-map* (kbd "XF86AudioPlay"))
  (undefine-key *top-map* (kbd "XF86AudioStop"))
  (undefine-key *top-map* (kbd "XF86AudioNext"))
  (undefine-key *top-map* (kbd "XF86AudioPrev")))

;; Filter out teams if possible
#++
(split-string
 (run-shell-command "playerctl -l" t))
