(define-module (scm-bin sgxsr)
  #:use-module (dotf utils)       ; str, module-name-for-logging, etc.
  #:use-module (dotf fs-utils)    ; dtfg
  #:use-module (guix build utils) ; which
  #:use-module (ice-9 optargs)    ; define*-public
  )

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ sgxsr) -s
!#

cd $dtf
./guix/home/common/scm-bin/sgxsr.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define common-lp         (str dtfg "/common"))
(define systems-common-lp (str dtfg "/systems/common"))
(define channels-scm      (str dtfg "/systems/common/syst-channels.scm"))

(define (beep-once)
  (let ((pid (primitive-fork)))
    (cond
      ((zero? pid)
       ;; Child: exec replaces the forked process so the captured pid IS
       ;; speaker-test — kill reaches it directly with no orphan risk.
       (let ((devnull (open-output-file "/dev/null")))
         (dup2 (fileno devnull) 1)
         (dup2 (fileno devnull) 2)
         (close-port devnull))
       (execlp "speaker-test" "speaker-test"
               "--test" "sine" "--frequency" "440")
       (primitive-exit 1))
      (else
       (usleep 220000)
       (false-if-exception (kill pid SIGKILL))
       (false-if-exception (waitpid pid))))))

(define (notify . notification-args)
  (when (and (equal? (getenv "XDG_CURRENT_DESKTOP") "XFCE")
             (let ((d (getenv "DISPLAY")) (w (getenv "WAYLAND_DISPLAY")))
               (or (and d (not (string-null? d)))
                   (and w (not (string-null? w)))))
             (which "notify-send"))
    ;; false-if-exception: notify-send can fail (no daemon, D-Bus error);
    ;; without this guard the script would abort before reconfigure runs.
    (false-if-exception
     (apply system* "notify-send" notification-args)))
  (when (which "speaker-test")
    (for-each (lambda (_) (beep-once)) '(1 2 3))))

(define*-public (main #:rest args)
  "Pull system channels, reconfigure the Guix system, roll back to
home-channels.  Extra arguments are forwarded to the initial `guix pull'."
  (let* ((extra-args (cdr args))
         (config (str dtfg "/systems/syst-" (gethostname) ".scm")))

    (unless (file-exists? config)
      (format (current-error-port)
              "No system config for host '~a': ~a\n"
              (gethostname) config)
      (exit 1))

    ;; No automatic --allow-downgrades for system channels.
    (let ((rc (status:exit-val
               (apply system* `("guix" "pull"
                                "--unsafe-channel-evaluation"
                                ,(string-append "--load-path=" common-lp)
                                ,(string-append "--channels=" channels-scm)
                                ,@extra-args)))))
      (unless (zero? rc) (exit rc)))

    (notify "Done" "`guix pull` finished")

    ;; The channels for system and home configurations may differ.  Roll back
    ;; the pull above to reactivate home-channels.  This won't behave correctly
    ;; if `guix pull' with system-channels was called more than once
    ;; consecutively, but it's faster than pulling the home-channels.
    ;; If the roll-back fails, use `gxp'.
    ;;
    ;; dynamic-wind guarantees the roll-back runs even if reconfigure fails,
    ;; mirroring the `trap ... EXIT' in the bash version.
    (let ((reconfigure-rc 0))
      (dynamic-wind
        (lambda () #f)
        (lambda ()
          ;; No sudo --login: the guix pull above already updated the current
          ;; environment; --login would source root's profile and pick up
          ;; root's older Guix generation instead.
          (set! reconfigure-rc
                (status:exit-val
                 (system* "sudo"
                          "guix" "system" "--verbosity=3" "--fallback"
                          (string-append "--load-path=" common-lp)
                          (string-append "--load-path=" systems-common-lp)
                          "reconfigure" config))))
        (lambda ()
          (let ((rollback-rc (status:exit-val
                              (system* "guix" "pull" "--roll-back"))))
            (exit (if (zero? reconfigure-rc)
                      rollback-rc
                      reconfigure-rc))))))))
(testsymb 'main)

(module-evaluated)
