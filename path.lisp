#|

silly snippets that I used to debug issues with my PATH environment
variable.

|#

(in-package #:stumpwmrc)

(split-string (getenv "PATH") ":")

#|

1. stumpwm's PATH already contains duplicates...
2. when login in into a tty (i.e. not x11), they are _not_ duplicated!

|#

(getenv "HOME_ENVIRONMENT")
(getenv "GUIX_PROFILE")
