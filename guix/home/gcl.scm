(define-module (gcl)
  #:use-module (utils)
  #:export (main gcl))

;; Examples:
#|
(source
 (origin
   (method url-fetch)
   (uri "http://www.example.com/3.2.1.tar.gz")
   (sha256 %null-sha256)))

(origin
  (method git-fetch)
  (uri (git-reference
        (url "http://www.example.com/x.git")
        (commit "0")))
  (sha256 %null-sha256))
|#

#|
TODO cloning git repos could be solved with origins:
(origin (method git-fetch) (uri (git-reference ...)) ...)

#!/usr/bin/guile \
-l utils.scm -e (gcl) -s
!#

;; $HOME variable can't be used

#!/home/bost/.guix-home/profile/bin/guile \
-l utils.scm -e (gcl) -s
!#

|#

(define* (gcl #:rest args)
  "Usage:
(gcl \"-f\" \"arg0\")
(gcl \"-f arg0\")
(equal? (gcl \"-f\" \"arg0\")
        (gcl \"-f arg0\"))
;; > #t
"
  (apply exec-system*
         "git" "clone"
         args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  (apply gcl (cdr args)))
