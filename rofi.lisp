(in-package #:stumpwmrc)

(defun rofi (mode)
  (run-shell-command (concat "rofi -show " mode
                             ;; " -m " (write-to-string (head-number (current-head)))
                             )))

(defcommand rofi-run () ()
  (rofi "run -sidebar-mode"))

(defcommand rofi-window () ()
  (rofi "window"))

(defcommand rofi-windowcd () ()
  (rofi "windowcd"))


(define-key *top-map* (menu-key "C-~A") "rofi-run")
(define-key *top-map* (menu-key "M-~A") "rofi-windowcd")
