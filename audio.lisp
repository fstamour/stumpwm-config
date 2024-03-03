
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

#++
(progn
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

  (let ((sink (car (sh "pactl get-default-sink"))))
    (sh "pactl get-sink-volume ~a" sink))

  (first '("Volume: front-left: 34025 /  52% / -17.08 dB,   front-right: 34025 /  52% / -17.08 dB"
           "        balance 0.00"))

  (let ((line "Volume: front-left: 34025 /  52% / -17.08 dB,   front-right: 34025 /  52% / -17.08 dB"))
    (parse-integer line :start (1+ (position #\/ line)) :junk-allowed t)))

(defun get-output ()
  (sh "pactl get-default-sink"))

(defun get-volume (&optional sink)
  (let* ((sink (or sink
                   (car (sh "pactl get-default-sink"))))
         (command-output (sh "pactl get-sink-volume ~a" sink))
         (volume-string (first command-output)))
    (when volume-string
      (parse-integer volume-string
                     :start (1+ (position #\/ volume-string))
                     :junk-allowed t))))

#++ (run-shell-command "amixer --device pulse get Master" t)

#++
(let ((sink (car )))
  (sh "pactl set-sink-volume ~a +5%" sink))

(defun set-volume (volume)
  ;; TODO validate volume
  ;; TODO pass device and control
  ;; TODO support jack?
  (let* ((sink (car (sh "pactl get-default-sink"))))
    (sh "pactl set-sink-volume ~a ~a" sink volume)
    (get-volume sink)))

(defun show-volume (&optional volume)
  (let ((volume (or volume (get-volume))))
    (if volume
        (sh "volnoti-show ~d" volume)
        (message "Failed to get the current volume"))))

(defcommand volume-up (&optional (step 2)) ()
  (set-volume (format nil "+~d%" step))
  (show-volume))

(defcommand volume-down (&optional (step 2)) ()
  (set-volume (format nil "-~d%" step))
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
