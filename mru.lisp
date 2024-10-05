
(in-package #:stumpwmrc)

#|

Trying to keep some kind of history, so that run-or-raise raises the
most recently used window first and then cycle through in
chronological order if called repeatedly.

I use two hooks to keep an list of windows in the Most Recently
Used (focused) order (hence the name of the file, mru.lisp).

*focus-window-hook* is called when a window is given focus. It is
called with 2 arguments: the current window and the last window (could
be nil).

*destroy-window-hook* is called whenever a window is destroyed or
withdrawn.

This variant of "run-or-raise" uses these variables for keeping states
between invocations:
- *focus-history* : the MRU list itself
- *cycle* : the current list of windows being cycled through
- *last-criteria* : the pair of "name" and "props" used to
filter (remove-if-not) the list of windows that made up the list in
*cycle* in the first place.

When using another command than "run-or-raise" to change the focus, I
want the next invocation of "run-or-raise" to raise the most recently
used window that matches. Long story short, I use a dynamic variable
*inside-my-run-or-raise-p* to detect whether the hooks
were "triggered" because of an invocation of "run-or-raise".

;; 2024-07-03 I found someone who made something very similar:
;; https://github.com/tslight/stumpwm/blob/main/modules/cycle-mru/cycle-mru.lisp

|#


;;; a very dumb "circular list"
;;;
;;; I don't use a real circular list, because it's less error prone
;;; this way (no need for *print-cycle*, for example).

(defclass cycle ()
  ((elements
    :initform nil
    :initarg :elements
    :accessor elements)
   (index
    :initform nil
    :initarg :index
    :accessor index))
  (:documentation "TODO"))

(defun cycle-empty-p (cycle)
  (or (null cycle)
      (zerop (length (elements cycle)))))

(defun cycle-next (cycle)
  "Move the cycle to the next element."
  (unless (cycle-empty-p cycle)
    (setf (index cycle) (mod (1+ (index cycle))
                             (length (elements cycle))))))

(defun cycle-get-current (cycle)
  "Get the current element."
  (unless (cycle-empty-p cycle)
    (elt (elements cycle) (index cycle))))

(defmethod print-object ((cycle cycle) stream)
  "Print a cycle, showing its current index and the whole list."
  (print-unreadable-object
      (cycle stream :type t :identity nil)
    (format stream "~s ~s"
            (index cycle)
            (elements cycle))))


;;; Dynamic variables

(defvar *focus-history* ()
  "The list of _all_ windows, the most-recently focused first.")

(defvar *last-criteria* nil
  "The name and props of the invocation of \"run-or-raise\" that initialized *cycle*.")

(defvar *cycle* ()
  "The current list of windows being cycled over.")

;; TODO if possible, use *executing-stumpwm-command* instead
(defvar *inside-my-run-or-raise-p* nil
  "Use inside the hooks, to detect whether the change of focus was triggered by \"my-run-or-raise\".")


;;; Hooks

(defun on-focus-window (new old)
  (declare (ignore old))
  (setf *focus-history* (delete new *focus-history* :count 1))
  (push new *focus-history*)
  (unless *inside-my-run-or-raise-p*
    (setf *last-criteria* nil
          *cycle* nil)))

(defun on-destroy-window (win)
  (setf *focus-history* (delete win *focus-history*))
  (when *cycle*
    (setf (elements *cycle*) (delete win (elements *cycle*)))
    (when (cycle-empty-p *cycle*)
      (setf *cycle* nil))))

(add-hook *focus-window-hook* 'on-focus-window)
(add-hook *destroy-window-hook* 'on-destroy-window)


;;; Putting it all together

;; TODO find a better name, it really is more of a "run-or-raise-of-cycle" ~ dwim
(defun my-run-or-raise (cmd props &optional (all-groups *run-or-raise-all-groups*)
                                    (all-screens *run-or-raise-all-screens*)
                        &aux
                          (cycle *cycle*)
                          (criteria (list cmd props)))
  (let ((*inside-my-run-or-raise-p* t))
    (if (and (not (cycle-empty-p cycle))
             (equal criteria *last-criteria*))
        (progn
          (cycle-next cycle)
          ;; TODO make sure that all matching windows are in the cycle
          ;; TODO I should use "matches" from below, and update the cycle accordingly
          (focus-window (cycle-get-current cycle) t))
        (let* ((current-window (current-window))
               (matches (stumpwm::find-matching-windows props all-groups all-screens))
               (matches-in-history (remove-if-not
                                    #'(lambda (win) (member win matches))
                                    (rest *focus-history*)))
               ;; other-matches is list of matches "after" the current
               ;; win, if current win matches. getting 2nd element means
               ;; skipping over the current win, to cycle through matches
               (other-matches (member current-window matches))
               (win (if (> (length other-matches) 1)
                        (second other-matches)
                        (first (or matches-in-history matches)))))
          (setf *last-criteria* criteria)
          (when win
            (setf *cycle* (make-instance 'cycle
                                         :elements matches
                                         :index (position win matches))))
          (if win
              (focus-window win t)
              (run-or-raise cmd props all-groups all-screens))))))

#++ ;; Select a window from the current cycle
(defcommand wip () ()
  (stumpwm::select-window-from-menu (elements *cycle*) "%m%n%s%50t"))

#++
(completing-read (current-screen)
                 "Choose:"
                 '("abc" "bcd" "cde"))

#++
(mapcar (lambda (win)
          (cons win (window-title win)))
        (elements *cycle*))



#++ ;; Like "my-run-or-raise", but only raise
(defun raise-window-dwim (cmd props &optional (all-groups *run-or-raise-all-groups*)
                                      (all-screens *run-or-raise-all-screens*)
                          &aux
                            (cycle *cycle*)
                            (criteria (list cmd props)))
  (let ((*inside-my-run-or-raise-p* t))
    (if (and (not (cycle-empty-p cycle))
             (equal criteria *last-criteria*))
        (progn
          (cycle-next cycle)
          ;; TODO make sure that all matching windows are in the cycle
          ;; TODO I should use "matches" from below, and update the cycle accordingly
          (focus-window (cycle-get-current cycle) t))
        (let* ((current-window (current-window))
               (matches (stumpwm::find-matching-windows props all-groups all-screens))
               (matches-in-history (remove-if-not
                                    #'(lambda (win) (member win matches))
                                    (rest *focus-history*)))
               ;; other-matches is list of matches "after" the current
               ;; win, if current win matches. getting 2nd element means
               ;; skipping over the current win, to cycle through matches
               (other-matches (member current-window matches))
               (win (if (> (length other-matches) 1)
                        (second other-matches)
                        (first (or matches-in-history matches)))))
          (setf *last-criteria* criteria)
          (when win
            (setf *cycle* (make-instance 'cycle
                                         :elements matches
                                         :index (position win matches)))
            (focus-window win t)
            win)))))
