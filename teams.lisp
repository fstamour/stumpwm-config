
(in-package #:stumpwmrc)

(defun list-microsoft-teams-windows ()
  "Find all Microsoft Teams chromium app windows."
  (remove-if-not
   (lambda (w)
     (and
      (stumpwm::classed-p w "Chromium-browser")
      (stumpwm::title-re-p w "^Microsoft Teams - .*")))
   (stumpwm::list-windows :screen)))

(defun get-microsoft-teams-windows ()
  (let ((windows (list-microsoft-teams-windows)))
    (when windows (first windows))))

(defcommand run-microsoft-teams () ()
  "Open a new Microsoft Teams chromium app."
  (run-commands "exec chromium --app-id=cifhbcnohmdccbgoicgdjpfamggdegmo"))

(defcommand teams-run-or-raise () ()
  "Start teams or switch to it if already running."
  (let ((window (get-microsoft-teams-windows)))
    (if windows
        (let ((win (first windows)))
          (focus-window win t))
        (run-microsoft-teams))))

#++
(defcommand teams-toogle-mic () ()
  "Send C-S-m to Microsoft Teams to toggle the microphone."
  (let ((windows (list-microsoft-teams-windows)))
    (if windows
        (let* ((win (first windows))
               (xwin (window-xwin win))
               (code 58)
               (state 21))
          (dolist (event '(:key-press :key-release))
            (xlib:send-event xwin
                             event
                             (xlib:make-event-mask event)
                             :display *display*
                             :root (screen-root
                                    (window-screen win))
                             ;; Apparently we need these in here, though they
                             ;; make no sense for a key event.
                             :x 0 :y 0 :root-x 0 :root-y 0
                             :window xwin
                             :event-window xwin
                             :code code
                             :state state)))
        (message "No Microsoft Teams window found."))))
