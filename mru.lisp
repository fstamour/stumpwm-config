
(in-package #:stumpwmrc)

#|

Work in progress: trying to keep some kind of history, so that
run-or-raise raises the most recently used window first and then cycle
through in chronological order if called repeateadly.


*focus-window-hook* is called when a window is given focus. It is
called with 2 arguments: the current window and the last window (could
be nil).

*destroy-window-hook* is called whenever a window is destroyed or
withdrawn.

|#

(defvar *focus-history* ())

(defun on-focus-window (new old)
  (delete new *focus-history* :count 1)
  (push new *focus-history*))

(defun on-destroy-window (win)
  (delete win *focus-history*))

(add-hook *focus-window-hook* 'on-focus-window)
(add-hook *destroy-window-hook* 'on-destroy-window)

(defcommand raise-latest () ()
  (when (second *focus-history*)
    (focus-window (second *focus-history*) t)))

(defun my-run-or-raise (cmd props &optional (all-groups *run-or-raise-all-groups*)
                                    (all-screens *run-or-raise-all-screens*))
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
    (if win
        (focus-window win t)
        (run-or-raise cmd props all-groups all-screens))))

;; Command and binding for testing only. It's currently disabled,
;; because I'm testing using my-run-or-raise in defapplication, which
;; means I always use my version.
#++
(progn
  (defcommand firefox2 () ()
    (my-run-or-raise "firefox" '(:class "firefox")))
  (define-key *root-map* (kbd "C-w") "firefox2"))
