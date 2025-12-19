
(in-package #:stumpwmrc)

(defun test (cmp got expected)
  (unless (funcall cmp got expected)
    (error "Test failed (~s): ~&expected: ~s~&     got: ~s~&"
           cmp expected got))
  got)


;;; testing docstring generation

(test 'string=
      (defapplication/docstring 'firefox 'run-or-raise)
      "Start firefox or switch to it if already running.")

(test 'string=
      (defapplication/docstring 'firefox 'new)
      "Starts a new instance of FIREFOX.")

(test 'string=
      (defapplication/docstring 'firefox 'windowlist)
      "Select a window for a list of firefox windows.")


(test 'equal
      (defapplication/command 'firefox 'run-or-raise nil nil nil)
      '(MY-RUN-OR-RAISE "firefox" '(:CLASS "Firefox")))


;;; testing without-earmuffs

(test 'equal
      (mapcar #'without-earmuffs
              '("a" "*a" "a*" "*a*"))
      '("a" "a" "a" "a"))


;;; WIP

(defapplication/command 'firefox 'new nil nil nil)
(defapplication/command 'firefox 'run-or-raise "ff" nil nil)
(defapplication/command 'firefox 'new "ff" nil nil)
(defapplication/command 'firefox 'run-or-raise "ff" "-w" '("-a"))

;; TODO this "compile/expands" without error (that I know of), but it's wrong.
(defapplication* keepassxc
    :bind (:root "k"))

(define-application *firefox*
  :classes '(or "firefox" "Firefox")
  (:bind (:root "w"))
  (:type new :bind (:root "W"))
  (:type windowlist :bind (:root "M-w")))
