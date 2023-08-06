
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


;;; Players

;; playerctl -l : e.g. teams == "chromium.instance5105"
(defun choose-player ()
  "Return the first player that isn't ms teasm -_-"
  (loop
    :with pids = (split-string (first (sh "pidof teams")) " ")
    :for player :in (sh "playerctl -l")
    :unless (find-if (lambda (pid)
                       (alexandria:ends-with-subseq pid player))
                     pids)
      :do (return player)))

;; (choose-player)

(defun call-with-player (fn)
  (let ((player  (choose-player)))
    (if player
        (funcall fn player)
        (message "No players found."))))

(defun playerctl (command)
  (call-with-player
   (lambda (player)
     (sh "playerctl ~a -p ~a" command player))))

;;; TODO sh is synchronous, not sure if I want that...
(defcommand music-toggle () ()
  (playerctl "play-pause"))

(defcommand music-next () ()
  (playerctl "next"))

(defcommand music-prev () ()
  (playerctl "prev"))

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
