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


(defun choose (choices &key (prompt "Choose"))
  "Wrap rofi to choose something."
  (let* ((hash-table (when (hash-table-p choices) choices))
         (alist (and (not hash-table)
                     (eq :alist (first choices))
                     (second choices)))
         (items (cond
                  (alist (mapcar #'cdr alist))
                  (hash-table (alexandria:hash-table-keys hash-table))
                  (t choices))))
    (with-input-from-string (input (let ((*print-case* :downcase))
                                     (format nil "~{~a~%~}" items)))
      (let ((choice (uiop:run-program `("rofi"
                                        ;; input from stdin, output to stdout
                                        "-dmenu"
                                        ;; case insensitive
                                        "-i"
                                        ;; normalize unicode with diacritics
                                        "-normalize-match"
                                        ;; allow pango markup
                                        ;; https://docs.gtk.org/Pango/pango_markup.html
                                        "-markup-rows"
                                        "-matching" "fuzzy"
                                        "-sort"
                                        "-sorting-method" "fzf"
                                        "-p" ,prompt)
                                      :input input
                                      :output '(:string :stripped t)
                                      :ignore-error-status t)))
        (unless (alexandria:emptyp choice)
          (cond
            (alist (car (rassoc choice alist :test #'string=)))
            (hash-table `(,choice ,@(gethash choice hash-table)))
            (t choice)))))))
