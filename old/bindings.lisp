
(in-package #:stumpwmrc)

;; Top map

(define-key *top-map* (kbd "s-SPC") "switch-keyboard-layout")

;; Root map  "C-t ..."

(define-key *root-map* (kbd "|") "toggle-split")
(define-key *root-map* (kbd "e") "exchange-direction left")

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


