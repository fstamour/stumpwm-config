
(in-package #:stumpwmrc)

;; Took directly from window-send-string
#+nil (defun string->keys (string)
	(mapcar (lambda (ch)
		  ;; exploit the fact that keysyms for ascii characters
		  ;; are the same as their ascii value.
		  (let ((sym (cond ((<= 32 (char-code ch) 127)
				    (char-code ch))
				   ((char= ch #\Tab)
				    (stumpwm-name->keysym "TAB"))
				   ((char= ch #\Newline)
				    (stumpwm-name->keysym "RET"))
				   (t (first (xlib:character->keysyms ch *display*))))))
		    (when sym
		      (stumpwm::send-fake-key window
					      (stumpwm::make-key :keysym sym)))))
		key-list-or-string))

;; TODO Use (stumpwm-name->keysym "...") see stumpwm/keysyms.lisp
;; (defun key-list-or-string->keys)

(defvar *hotkeys*
  `((:phone "5555555555")))

(defcommand hotkey (&optional hotkey-id (window (current-window))) ()
  (let ((hotkey-id (or hotkey-id
		       (cdr (select-from-menu (current-screen)
					      (mapcar (lambda (hotkey-spec)
							(cons
							 (string-downcase (car hotkey-spec))
							 (car hotkey-spec)))
						      *hotkeys*)
						 "Which hotkey?")))))
    (when hotkey-id
      (window-send-string (second (assoc hotkey-id *hotkeys*))))))

(define-key *top-map* (kbd "s-x") "hotkey")

