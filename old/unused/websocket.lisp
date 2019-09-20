
;; (ql:quickload :hunchensocket)
;; (ql:quickload :cl-json)

(defpackage #:stumpwmrc.websocket
  (:use #:cl
	#:stumpwm
	#:stumpwmrc)
  (:import-from :hunchensocket
		websocket-resource
		websocket-client))

(in-package :stumpwmrc.websocket)

(defclass stumpwm-resource (websocket-resource) ())

(defvar *websocket-ressource* (make-instance 'stumpwm-resource)))

(defun find-websocket-ressource (request)
  (when (string= "/" (hunchentoot:script-name request))
	       *websocket-ressource*))

(pushnew 'find-websocket-ressource hunchensocket:*websocket-dispatch-table*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun broadcast-verbatim (websocket-resource message)
  "Send a formatted message to all clients."
  (loop :for peer :in (hunchensocket:clients websocket-resource)
     :do (hunchensocket:send-text-message peer message)))

(defun broadcast (websocket-resource message &rest args)
  "Send a formatted message to all clients."
  (broadcast-verbatim websocket-resource (apply #'format nil message args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod hunchensocket:client-connected ((res stumpwm-resource) user)
  (message"Connection from ~A" (hunchensocket::remote-addr (hunchensocket:client-request user))))

#+ (or) (defmethod hunchensocket:client-disconnected ((room chat-room) user)
	  (broadcast room "~a has left ~a" (name user) (name room)))

#+ (or) (defmethod hunchensocket:text-message-received ((room chat-room) user message)
	(message room "~a says ~a" (name user) message))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Delcare the server.
(defvar *server* (make-instance 'hunchensocket:websocket-acceptor :port 8084))

;; Start the server.
(hunchentoot:start *server*)

(in-package :stumpwm)

(defvar *window-included-slots* 
  '(;xwin    
    width   
    height  
    x       
    y       
    ;gravity 
    ;group   
    ;number  
    ;parent  
    title   
    ;user-title
    class   
    ;type    
    ;res     
    ;role    
    ;state   
    ;marked  
    ;plist   
    ;fullscreen
    )))

(in-package :stumpwmrc.websocket)

(defmethod cl-json:encode-json ((o window) &optional (stream cl-json:*json-output*))
  (cl-json:with-object (stream)
    (let ((streamer (cl-json:stream-object-member-encoder stream)))
      (cl-json::map-slots
       (lambda (name value)
	 (when (member name stumpwm::*window-included-slots*)
	   (funcall streamer name value)))
       o))))

#+dev-test (cl-json:encode-json-to-string (current-window))
#+dev-test (format t "\"~A\"" (cl-json:encode-json-to-string (current-window)))

(defun broadcast-focus-changed (to-window from-window)
  (declare (ignorable from-window))
  (broadcast-verbatim *websocket-ressource*
		      (cl-json:encode-json-to-string to-window)))

(add-hook *focus-window-hook* 'broadcast-focus-changed)
;; (stumpwm:remove-hook stumpwm:*focus-window-hook* 'broadcast-focus-changed)


