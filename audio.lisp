
(in-package #:stumpwmrc)

;; sanitiy check:
;; (parse-integer "" :junk-allowed t) ; => nil
;; (parse-integer nil :junk-allowed t) ; => error

;; TODO check if playerctl, amixer, grep
;; TODO *alsa-device* and *alsa-control*

;; (sh "echo $SHELL")
;; => fish

;; Check if "amixer" is available
;; (sh "bash -c 'command -v ~a'" "amixer")

(defparameter *audio-control-style* :amixer)

(defun amixerp ()
  (eq *audio-control-style* :amixer))

(defun pulse-audio-p ()
  (eq *audio-control-style* :pulseaudio))

(defparameter *last-sh* nil
  "For debugging purpposes only: keep information about the last time the function sh was called.
Trace could be useful too (especially that this only keeps the last invocation.0")

(defun sh (control-string &rest format-arguments)
  "Run a command in a shell, wait for it, return its output as a list of lines (strings)."
  (setf *last-sh* (list control-string format-arguments)) ; for debugging
  (let* ((command (apply #' format nil control-string format-arguments))
         (result (uiop:split-string
                  #++ (run-shell-command command t)
                  (uiop:run-program
                   command
                   :output '(:string :stripped t))
                  :separator '(#\Newline))))
    (prog1 result
      ;; More bebugging stuff
      (setf (cdr (last *last-sh*))
            (list command result)))))

#++
(progn
  (sh "amixer get Master" t)
  (sh "amixer --device pulse get Master" t))

#++
(progn
  (let ((sink (car (sh "pactl get-default-sink"))))
    (sh "pactl get-sink-volume ~a" sink))

  (first '("Volume: front-left: 34025 /  52% / -17.08 dB,   front-right: 34025 /  52% / -17.08 dB"
           "        balance 0.00"))

  (let ((line "Volume: front-left: 34025 /  52% / -17.08 dB,   front-right: 34025 /  52% / -17.08 dB"))
    (parse-integer line :start (1+ (position #\/ line)) :junk-allowed t)))

(defun get-output ()
  (if (pulse-audio-p)
      (sh "pactl get-default-sink")
      (error "Tried to call \"get-output\" when pulse-audio is disabled.")))

(defun get-volume (&optional sink)
  (ccase *audio-control-style*
    (:amixer
     (let* ((control "Master")
            ;; command-output is a list of lines, the grep keeps only the
            ;; percentages. For example, if you have 2 "outputs" Left and
            ;; Right, both at 40% the list will be '(40 40).
            (command-output
              (sh
               "amixer get ~a | grep -Po \"[0-9]+(?=%)\""
               control))
            (volume-string (first command-output)))
       (when volume-string
         (parse-integer volume-string :junk-allowed t))))
    (:pulseaudio
     (let* ((sink (or sink
                      (car (sh "pactl get-default-sink"))))
            (command-output (sh "pactl get-sink-volume ~a" sink))
            (volume-string (first command-output)))
       (when volume-string
         (parse-integer volume-string
                        :start (1+ (position #\/ volume-string))
                        :junk-allowed t))))))

#++
(get-volume)

(defun set-volume (volume)
  ;; TODO validate volume
  ;; TODO pass device and control
  ;; TODO support jack?
  (ccase *audio-control-style*
    (:amixer
     (let* ((control "Master"))
       (sh "amixer sset ~a ~d" control volume)
       (get-volume)))
    (:pulseaudio
     (let* ((sink (car (sh "pactl get-default-sink"))))
       (sh "pactl set-sink-volume ~a ~a" sink volume)
       (get-volume sink)))))

(defun show-volume (&optional volume)
  (let ((volume (or volume (get-volume))))
    (if volume
        #++ ;; TODO I don't have volnoti on GuixSD :/
        (sh "volnoti-show ~d" volume)
        (message "Volume: ~d" volume)
        (message "Failed to get the current volume"))))

(defcommand volume-up (&optional (step 2)) ()
  (set-volume (format nil
                      (ccase *audio-control-style*
                        (:amixer "~d%+")
                        (:pulseaudio "+~d%"))
                      step))
  (show-volume))

(defcommand volume-down (&optional (step 2)) ()
  (set-volume (format nil
                      (ccase *audio-control-style*
                        (:amixer "~d%-")
                        (:pulseaudio "-~d%"))
                      step))
  (show-volume))

#++
(progn
  (get-volume)
  (set-volume "30%")
  (show-volume)
  (volume-up)
  (volume-down))

#++ ;; TODO
(defcommand mute-toggle () ()
  (run-shell-command "amixer set Master toggle"))

;; for testing: (mute-toggle)

(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")
#++ (define-key *top-map* (kbd "XF86AudioMute") "mute-toggle")


;;; Players



(defparameter *player-filters* nil
  "Function to filter out players when calling list-players.")

(setf *player-filters*
      (list
       (lambda (player)
         ;; I only use chrome (chromium in fact) for MS Teams, which I
         ;; don't want it considered as a player.
         (alexandria:starts-with-subseq "chrom" player))))

(defun list-players (&optional (filters *player-filters*))
  (remove-if (lambda (p)
               (some (lambda (f) (funcall f p))
                     (append filters (list #'alexandria:emptyp))))
             (sh "playerctl -l")))

#++
(list
 (list-players)
 (list-players nil))

(defparameter *last-player* nil)

(defun choose-player ()
  "Return the first player that isn't ms teasm -_-"
  ;; TODO use *last-player*
  (first (list-players)))

;; (choose-player)

;; TODO bind prefix-(kbd "XF86AudioNext") to next-player


(defun call-with-player (fn)
  (let ((player (choose-player)))
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
