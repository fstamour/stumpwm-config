;; -*-lisp-*-

(in-package #:stumpwmrc)

(defvar *stumpwm-contrib* (merge-pathnames
			   (make-pathname
			    :directory '(:relative "extern-dependencies" "stumpwm-contrib"))
			   *stumpwmrc-directory*)
  "Path to stumpwm-contrib git repository.xs")

(defun make-contrib-dir (&rest paths)
  (merge-pathnames 
   (make-pathname
    :directory (append '(:relative) paths))
   *stumpwm-contrib*))

(add-to-load-path
 (make-contrib-dir "util" "stumptray"))

;; Needed by stumptray
;; (ql:quickload :xembed)

(load-module "stumptray")
(stumptray)

;; (stumpwm::list-modules)
;; *load-path*

;; (load-module "cpu")
;; (load-module "wifi")
;; (load-module "net")

;; (list-modules)
