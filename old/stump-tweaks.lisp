
(in-package :stumpwm) ;; <-- @note Here we redifine thing IN the stumpwm pacakge.

;; @todo Check if the one returned is the current window, if it is switch head
(defun find-matching-windows (props all-groups all-screens &optional (this-head-first t))
  "Returns list of windows matching @var{props} (see run-or-raise
documentation for details). @var{all-groups} will find windows on all
groups. Same for @{all-screens}. Result is sorted by group and window
number, with group being more significant (think radix sort)."
  (let* ((screens (if all-screens
                      *screen-list*
                      (list (current-screen))))
         (winlist (if all-groups
		      (mapcan (lambda (s) (screen-windows s)) screens)
		      (group-windows (current-group))))
         (matches (remove-if-not (lambda (w)
                                   (apply 'window-matches-properties-p w props))
                                 winlist)))
    (let ((result (stable-sort (sort matches #'< :key #'window-number)
			       #'< :key (lambda (w) (group-number (window-group w))))))
      (when this-head-first
	(let* ((head (current-head))
	       (new-winlist (remove-if-not (lambda (w)
					     (eq head (window-head w)))
					   result)))
	  (when new-winlist
	    (setf result new-winlist))))
      result)))



;; Re-define the describe-key command, so that the key sequence is copied into the clipboard.
(defcommand describe-key (keys) ((:key-seq "Describe Key: "))
  "Either interactively type the key sequence or supply it as text. This
command prints the command bound to the specified key sequence."
  (let* ((cmd (loop for map in (top-maps)
                 for cmd = (lookup-key-sequence map keys)
                 when cmd return cmd))
         (key-seq (format nil "~{~a~^ ~}" (mapcar 'print-key keys)))
         (kbd-form (format nil "(kbd \"~a\")" key-seq)))
    (set-x-selection kbd-form)
    (if cmd
        (message "~a is bound to \"~a\" (~s copied to clipboard)." key-seq  cmd kbd-form)
        (message "~a is not bound. (~s copied to clipboard)" key-seq kbd-form))))
