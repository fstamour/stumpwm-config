;;;; N.B. This doesn't _need_ to reside in stumpwm, it's just more
;;;; convenient than having some kind of deamon.

(in-package #:stumpwmrc)

(defun read-bookmarks (path &optional destination)
  (with-hash-table (destination)
    (with-tsv (path cells)
      (destructuring-bind (key value)
          cells
        (add key (list :url value))))))

(defun read-searches (path &optional destination)
  (with-hash-table (destination)
    (with-tsv (path cells)
      (destructuring-bind (key url-template &optional description)
          cells
        (add key (list :search url-template description))
        ;; This gives a nicer display... but the matching is much worst
        ;; (add (format nil "~a~@[ - Search ~a~]" key description) (list :search url-template description))
        ))))

;; TODO put "bookmarks" into $XDG_DATA_HOME
(defun bookmarks-path ()
  (merge-pathnames "dev/meta/dotfiles/data/bookmarks" (user-homedir-pathname)))

;; TODO put "searches" into $XDG_DATA_HOME
(defun searches-path ()
  (merge-pathnames "dev/meta/dotfiles/data/searches2" (user-homedir-pathname)))

(defun get-repositories (&optional destination)
  (with-hash-table (destination)
    (mapcar (lambda (repo)
              (add repo (list :repo)))
            (emacs-list-repositories))))

(defun list-choices ()
  (let ((choices (make-hash-table :test 'equal)))
    (with-hash-table (choices)
      (add "Open searches" (list :path (namestring (searches-path))))
      (add "Open bookmarks" (list :path (namestring (bookmarks-path))))
      (read-bookmarks (bookmarks-path) choices)
      (read-searches (searches-path) choices)
      (get-repositories choices))))

;; (time (list-choices))



(defun handle-repo (pathname)
  (let* ((choices '((emacs-magit . "Open repository in emacs with magit (magit-status).")
                    (emacs-dired . "Open the repository in emacs with dired.")
                    (forge-issues . "Open the issue tracker in the forge.")
                    (forge-repo . "Open the repository in the forge.")
                    #|
                    Other ideas:
                    - open in tmux
                    - open in new terminal
                    - open with projectile (will select a file with projectile)
                    - open the upstream remote's in its forge.
                    |#))
         (action (choose (list :alist choices)
                         :prompt (format nil "What do you want to do with the repo ~s?" pathname))))
    (ecase action
      (emacs-magit (emacs-magit pathname))
      (emacs-dired (emacs-find-file pathname))
      (forge-issues #|TODO|#)
      (forge-repo #|TODO|#))))

#++ (handle-repo (merge-pathnames "dev/lem" (user-homedir-pathname)))

(defun handle-search (keyword url-template description)
  (let ((needle (choose () :prompt (or description keyword))))
    (when needle
      (xdg-open (uiop:frob-substrings url-template '("%s") needle)))))

#++ (handle-search "man" "https://man.archlinux.org/search?q=%s" nil)

(defcommand quicksearch () ()
  (destructuring-bind (&optional user-input type value extra)
      (choose (list-choices))
    (cond
      ((null user-input) #| nothing to do |#)
      (type
       (case type
         (:url (xdg-open value))
         (:search (handle-search user-input value extra))
         (:repo (handle-repo user-input))
         (:path (emacs-find-file value))))
      ((probe-file user-input) (emacs-find-file user-input))
      (t (error "quicksearch: don't know how to handle ~s"
                (list :user-input user-input
                      :type type
                      :value value
                      :extra extra))))))

(define-key *root-map* (kbd "q") "quicksearch")


