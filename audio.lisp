
(in-package #:stumpwmrc)

(defcommand volume-down () ()
  (run-shell-command
   #-freebsd
   "amixer set Master 500-"
   #+freebsd
   "mixer vol -5"))

(defcommand volume-up () ()
  (run-shell-command
   #-freebsd
   "amixer set Master 500+"
   #+freebsd
   "mixer vol +5"))

#+freebsd
(defun current-volume ()
  "On FreeBSD, get the current master volume."
  (ppcre:register-groups-bind
      ((#'parse-integer lvol rvol))
      ("(\\d+):(\\d+)"
       (run-shell-command "mixer vol" t)
       :sharedp t)
    (values lvol rvol)))

(defparameter *volume-before-muting* nil
  "Variable used to restore to volume to its previous level.")

;; No easy mute on freeBSD for now
(defcommand mute-toggle () ()
  #-freebsd
  (run-shell-command "amixer set Master toggle")
  #+freebsd
  (let ((current-volume (current-volume)))
    (if (zerop current-volume)
        ;; Currently muted, unmute
        ;; TODO what if *volume-before-muting* is nil?
        (run-shell-command (format nil "mixer ~d"
                                   *volume-before-muting*))
        ;; Not muted, muting
        (progn
          (setf *volume-before-muting* current-volume)
          (run-shell-command "mixer 0")))))

;; for testing: (mute-toggle)

(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")
(define-key *top-map* (kbd "XF86AudioMute") "mute-toggle")

;;
#|
(define-key *top-map* (kbd "XF86AudioPlay") "music-toggle") ;
(define-key *top-map* (kbd "XF86AudioStop") "music-stop") ;
(define-key *top-map* (kbd "XF86AudioNext") "music-next") ;
(define-key *top-map* (kbd "XF86AudioPrev") "music-prev") ;
|#

#|
(progn                                  ;
(undefine-key *top-map* (kbd "XF86AudioPlay")) ;
(undefine-key *top-map* (kbd "XF86AudioStop")) ;
(undefine-key *top-map* (kbd "XF86AudioNext")) ;
(undefine-key *top-map* (kbd "XF86AudioPrev"))) ;
|#
