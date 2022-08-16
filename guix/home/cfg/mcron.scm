#|

guix shell --development guix help2man git strace --pure
./pre-inst-env guix repl

(add-to-load-path
 (string-append (getenv "HOME") "/dev/guix"))

|#

(define-module (cfg mcron)
  ;; #:use-module (guix gexp)              ;; #| #~ #$ etc. |#
  #:export ())

;; https://github.com/clojure-quant/infra-guix/blob/main/home/config-nuc.scm

;; (define do-job
;;   ;; as user "bost" at 17:05 This runs from the user's home directory.
;;   ;; #~(job
;;   ;;    '(next-minute-from (next-hour '(17)) '(5))
;;   ;;    (invoke "touch"
;;   ;;            (string-append "/tmp/srvc-second-"
;;   ;;                           (number->string (current-time))))
;;   ;;    #:user "bost")

;;   #~(job '(next-second)
;;          ;; (lambda () ...) doesn't work
;;          (list
;;           (invoke "touch"
;;                   (string-append "/tmp/srvc-second-"
;;                                  (number->string (current-time))))))
;;   )

(define srvc-singleton-<time>
  '(lambda ()
     ;; (lambda () ...) doesn't work (when NOT IN a gexp?)
     ;; (list ...) doesn't work when IN a gexp?
     (invoke "touch"
             (string-append "/tmp/srvc-singleton-"
                            (number->string (current-time))))
     ;; get the pid of the parent process and kill that process;
     ;; i.e. effectively kill this job-process
     (kill (getppid) SIGINT)))

(define srvc-multi-<time>
  '(lambda ()
     ;; (lambda () ...) doesn't work (when NOT IN a gexp?)
     ;; (list ...) doesn't work when IN a gexp?
     (invoke "touch"
             (string-append "/tmp/srvc-multi-"
                            (number->string (current-time))))))

(define srvc-touch-once
  '(lambda ()
     (system "touch /tmp/srvc-touch-once")
     ;; get the pid of the parent process and kill that process;
     ;; i.e. effectively kill this job-process
     (kill (getppid) SIGINT)))

(define mcron-service
  ;; TODO test if the command-string can be created by string-append
  (service
   home-mcron-service-type
   (home-mcron-configuration
    (jobs
     (let* (;; every second
            ;; (job-period '(next-second))
            ;; every 5 seconds
            (job-period '(next-second (range 0 60 5))))
       (list
        ;; see '(gexp->derivation "the-thing" build-exp)' in the manual
        ;; Also: #+ vs. #$
        ;; In a cross-compilation context, it is useful to distinguish
        ;; between references to the native build of a package—that can
        ;; run on the host—versus references to cross builds of a package.
        ;; To that end, the #+ plays the same role as #$, but is a
        ;; reference to a native package build

        ;; #~(job '#$job-period '#$srvc-singleton-<time>)
        ;; #~(job '#$job-period '#$srvc-multi-<time>)
        ;; #~(job '#$job-period '#$srvc-touch-once)
        #~(job '#$job-period (lambda ()
                               ;; (lambda () ...) doesn't work (when NOT IN a gexp?)
                               ;; (list ...) doesn't work when IN a gexp?
                               ;; TODO this doesn't get invoked at ALL!!!
                               (invoke "touch"
                                       (string-append "/tmp/srvc-job-singleton-"
                                                      (number->string (current-time))))
                               ;; get the pid of the parent process and kill that process;
                               ;; i.e. effectively kill this job-process
                               (kill (getppid) SIGINT)))
        #~(job '#$job-period (lambda ()
                               ;; (lambda () ...) doesn't work (when NOT IN a gexp?)
                               ;; (list ...) doesn't work when IN a gexp?
                               ;; TODO this doesn't get invoked at ALL!!!
                               (invoke "touch"
                                       (string-append "/tmp/srvc-job-multi-"
                                                      (number->string (current-time))))))
        #~(job '#$job-period (lambda ()
                               ;; TODO this doesn't get killed
                               (system "touch /tmp/srvc-job-lambda-touch-once")
                               ;; get the pid of the parent process and kill that process;
                               ;; i.e. effectively kill this job-process
                               (kill (getppid) SIGINT)))
        #~(job '#$job-period "touch /tmp/srvc-job-string-touch-periodically-0")
        ))))))

