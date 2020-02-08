
(in-package #:stumpwmrc)

;; Top map "as is"

(define-key *top-map* (kbd "s-l") "lock")
(define-key *top-map* (kbd "s-SPC") "switch-keyboard-layout")

(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")
(define-key *top-map* (kbd "XF86AudioMute") "mute-toggle")

(define-key *top-map* (kbd "XF86AudioPlay") "music-toggle")
(define-key *top-map* (kbd "XF86AudioStop") "music-stop")
(define-key *top-map* (kbd "XF86AudioNext") "music-next")
(define-key *top-map* (kbd "XF86AudioPrev") "music-prev")

(define-key *top-map* (kbd "s-Right") "move-window right")
(define-key *top-map* (kbd "s-Left") "move-window left")
(define-key *top-map* (kbd "s-Up") "move-window up")
(define-key *top-map* (kbd "s-Down") "move-window down")

;; Root map  "C-t ..."

(define-key *root-map* (kbd "|") "toggle-split")
(define-key *root-map* (kbd "e") "exchange-direction left")

(define-key *top-map* (kbd "C-Menu") "send-escape")

;; App Root map

(defun def-app-key (key command)
  (define-key *app-root-map* key command))

(def-app-key (kbd "Menu") "exec dmenu_run -i -b")
(def-app-key (kbd "j") "intellij")
(def-app-key (kbd "t") "terminal")

(def-app-key (kbd "h") "toggle-hdmi")

;; TODO (if loaded "xscreensaver"... (something like that)
(def-app-key (kbd "s") "activate-xscreensaver")
(def-app-key (kbd "S") "config-xscreensaver")


