
(in-package #:stumpwmrc)

(defcommand volume-down () ()
  (run-shell-command "amixer set Master 500-"))

(defcommand volume-up () ()
  (run-shell-command "amixer set Master 500+"))

(defcommand mute-toggle () ()
  (run-shell-command "amixer set Master toggle"))

(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")
(define-key *top-map* (kbd "XF86AudioMute") "mute-toggle")

(define-key *top-map* (kbd "XF86AudioPlay") "music-toggle")
(define-key *top-map* (kbd "XF86AudioStop") "music-stop")
(define-key *top-map* (kbd "XF86AudioNext") "music-next")
(define-key *top-map* (kbd "XF86AudioPrev") "music-prev")


