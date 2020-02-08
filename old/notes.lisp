;;
;; Rename commands to "note-*" to avoid name conflict.
;;
;; Being able to edit/tweaked last saved note.
;; DONE. Manually enter tags
;; Manually enter things into the notes
;; Suggest tags!!
;; See  ~/quicklisp/local-projects.bak/bookmark/db/

(in-package :stumpwmrc)

(defpackage :swm.notes
  (:use :cl
	:stumpwm
	:stumpwmrc))

(in-package :swm.notes)

(defun uri-p (string)
  "Check if the string is an uri, needs a lot of tweaking"
  (when (typep string 'string)
    (string= string "http" :end1 3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *tags* '(stumpwm))

(defparameter *current-note* '())
(defparameter *current-topic* '()
  "A \"topic\" is a list of tags.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcommand note-show-current () ()
  (if *current-note*
      (message "~&Current note: ~&~{s\"~A\"~&~}" (reverse *current-note*))
      (message "Current note is empty")))

(defcommand note-append-x-selection () ()
  (let ((sel (get-x-selection))
	(ccn (car *current-note*))
	(message-fmt "")
	(message-args '()))
    (flet ((msg-append (fmt &rest args)
	     (setf message-fmt (strcat message-fmt fmt)
		   message-args (append args message-args))))
      (if (plusp (length sel))
	  (progn
	    (if (or
		 (not (stringp ccn))
		 (string/= sel ccn))
		(push sel *current-note*)
		(msg-append "~&^B^1Duplicated entry, doing nothing.^*"))
	    (msg-append "~&Current note: ~&~{~A~&~}" (reverse *current-note*)))
	  (msg-append "~&^B^1Empty selection"))
      
      (apply #'message message-fmt (reverse message-args)))))

(defcommand note-append (note) ((:string "Add to note: "))
  (if (plusp (length note))
      (progn
	(push note *current-note*)
	(note-show-current))
      (progn
	(message "~&^B^1Empty string"))))

(defun %add-tag (tag)
  (if (plusp (length tag))
      (let ((datum (list :tag tag)))
	(push datum *current-note*)
	(message "Added ~A to current note." datum))
      (message "~&^B^1Empty tag")))

(defcommand note-add-tag-x-selection () ()
  (let ((sel (get-x-selection)))
    (%add-tag sel)))

(defcommand note-add-tag (tag) ((:string "Add a tag: "))
  (%add-tag tag))


(defcommand note-finish () ()
  ;; Groups all tags
  (if *current-note*
      (progn
	(push (list :universal-time (get-universal-time)) *current-note*)
	(save-note *current-note*)
	(message "Note saved.")
	(setf *current-note* '()))
      (message "^B^1Current note is empty. Nothing to do.")))

(defun save-note (note)
  (with-open-file (notes-file-stream "~/Dropbox/swm-notes.sexp"
				     :if-exists :append
				     :if-does-not-exist :create
				     :direction :output)
    (print note notes-file-stream)))


(defcommand undo () ()
  (if *current-note*
      (progn
	(pop *current-note*)
	(note-show-current))
      (message "^B^1Invalid: current note is empty.")))


(defcommand note-delete () ()
  (setf *current-note* '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This was a nice idea, but it doesnt' work because of Java (probably).
#+ (or) (defcommand freemind/insert/uri-title () ()
	(unless (eq 2 (length *current-note*))
	  (error "Current note is not of length 2"))
	(let* (title uri)
	  (if (uri-p (car *current-note*))
	      (setf uri (first *current-note*)
		    title (second *current-note*))
	    (setf title (first *current-note*)
		  uri (second *current-note*)))
	  ;; TODO Some verifications ?
	  (let ( ;; Maybe swith to freemind...
		(window (current-window)))
	    (window-send-string (format nil "~%asdf~%") window))
	  #+nil(message "Title: ~S, uri: ~S" title uri)))

;; TODO 
(defcommand note-to-clipboard () ()
  (if *current-note*
      (set-x-selection (cat* *current-note*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *notes-root-map* (make-sparse-keymap))
(define-key *top-map* (kbd "s-z") *notes-root-map*)

(loop :for (key command) :in '(("a" "note-append-x-selection")
			       ("A" "note-append")
			       ;; ("b" "freemind/insert/uri-title")
			       ("f" "note-finish")
			       ("s" "note-show-current")
			       ("t" "note-add-tag")
			       ("T" "note-add-tag-x-selection")
			       ("z" "undo")
			       ;; ("w" "save-note")
			       )
   :do
   (define-key *notes-root-map* (kbd key) command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Viewer...




