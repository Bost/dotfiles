(define-module (scm-bin extract)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  ;; the (ice-9 regex) might need to be added in the home-configuration.scm in the macro
  #:use-module (ice-9 regex)
  #:export (main extract))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ extract) -s
!#

cd $dotf
./guix/home/common/scm-bin/extract.scm 

|#

(evaluating-module)

(define* (extract #:rest args)
  "Usage:
(extract ...)
"
  (call/cc
   (lambda (continuation)
     ((comp
       (partial apply exec-system*)
       ;; (lambda (a) (format #t "2 file: ~a\n" a) a)
       (lambda (a)
         (append
          (cond
           [(string-match ".*\\.tar.gz$" a)
            ;; if test $status -eq 2
            ;;   "WARN: The archive is propably is a tar, not a GZip. Try instead:"
            ;;   (list "tar" "xvf")
            ;; end
            (list "tar" "xzf")]

           [(string-match ".*\\.tar.bz2$" a)  (list "tar" "xjf")]
           [(string-match ".*\\.bz2$" a)      (list "bunzip2")]
           [(string-match ".*\\.rar$" a)      (list "unrar" "e")]
           [(string-match ".*\\.gz$" a)       (list "gunzip")]
           [(string-match ".*\\.tar$" a)      (list "tar" "xf")]
           [(string-match ".*\\.tbz2$" a)     (list "tar" "xjf")]
           [(string-match ".*\\.tgz$" a)      (list "tar" "xzf")]
           [(string-match ".*\\.jar$" a)      (list "unzip")]
           [(string-match ".*\\.war$" a)      (list "unzip")]
           [(string-match ".*\\.zip$" a)      (list "unzip")]
           [(string-match ".*\\.Z$" a)        (list "uncompress")]
           [(string-match ".*\\.7z$" a)       (list "7z" "x")]
           [(string-match ".*\\.tar\\.xz$" a) (list "tar" "xvfJ")]
           [#t (continuation
                (format #t "ERROR. No match for '~a'\n" a))]
           )
          (list a)))
       ;; (lambda (a) (format #t "1 file: ~a\n" a) a)
       car
       ;; (lambda (a) (format #t "0 args: ~a\n" a) a)
       )
      args))))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" ...)"
  ((comp
    (partial apply extract)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
