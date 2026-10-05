(define-module (scm-bin sgxsr)
  #:use-module (bost common utils) ; str, module-name-for-logging, etc.
  #:use-module (dotf fs-utils)     ; dtfg
  #:use-module (guix build utils)  ; which
  #:use-module (ice-9 optargs)     ; define*-public
  )

#|

#!/usr/bin/env -S guix repl -L ./ -L ./guix/common --
!#

cd $dotf && echo -e "\n(apply main (command-line))" >> ./guix/home/common/scm-bin/sgxsr.scm
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

(define (parse-command-args args)
  ;; Keep each shell argument intact; ungrouped arguments go to guix system.
  (let loop ((args args) (group 'system) (pull '()) (system '()))
    (if (null? args)
        (cons (reverse pull) (reverse system))
        (let ((arg (car args)))
          (cond
           ((string=? arg "--no-pull")
            (loop (cdr args) group pull system))
           ((string=? arg "--args-pull")
            (loop (cdr args) 'pull pull system))
           ((string=? arg "--args-system")
            (loop (cdr args) 'system pull system))
           ((eq? group 'pull)
            (loop (cdr args) group (cons arg pull) system))
           (else
            (loop (cdr args) group pull (cons arg system))))))))

(define*-public (main #:rest args)
  "Pull system channels, reconfigure the Guix system, roll back to
home-channels.

`--args-pull' collects arguments for the first `guix pull'.
`--args-system' collects arguments for `guix system'.  Each group runs
until the next group marker or the end of the command line.  Arguments
before either marker are forwarded to `guix system'.  Repeated groups
append arguments in order.  The rollback receives no extra arguments.

Examples:
  sgxsr --args-pull --allow-downgrades --args-system --dry-run
  sgxsr --no-pull --args-system --dry-run

`--no-pull' skips both pull and rollback and cannot be combined with
`--args-pull', even when that argument group is empty.

TODO Implement `sgxsr --equal-to-home-channels'
1. comment out existing in syst-channels.scm
2. add the channels from home-channels.scm
3. prepend timestamp
"
 (let* ((no-pull (member "--no-pull" (cdr args)))
        (command-args (parse-command-args (cdr args)))
        (pull-args (car command-args))
        (system-args (cdr command-args))
        (config (str dtfg "/systems/syst-" (gethostname) ".scm")))

    (when (and no-pull (member "--args-pull" (cdr args)))
      (format (current-error-port)
              "sgxsr: --no-pull cannot be combined with --args-pull\n")
      (exit 1))

    (unless (file-exists? config)
      (format (current-error-port)
              "No system config for host '~a': ~a\n"
              (gethostname) config)
      (exit 1))

    (unless no-pull
      ;; No automatic --allow-downgrades for system channels.
      (let ((rc (status:exit-val
                 (apply system*
                        `("guix" "pull"
                          "--unsafe-channel-evaluation"
                          ,(string-append "--load-path=" common-lp)
                          ,(string-append "--channels=" channels-scm)
                          ,@pull-args
                          )))))
        (unless (zero? rc) (exit rc)))

      (notify "Done" "`guix pull` finished"))

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
                 (apply system*
                        `("sudo" "guix" "system"
                          ,@system-args
                          "--verbosity=3" "--fallback"
                          ,(string-append "--load-path=" common-lp)
                          ,(string-append "--load-path=" systems-common-lp)
                          "reconfigure" ,config
                          )))))
        (lambda ()
          (unless no-pull
            (let ((rollback-rc (status:exit-val
                                (apply system*
                                       `("guix" "pull" "--roll-back")))))
              (exit (if (zero? reconfigure-rc)
                        rollback-rc
                        reconfigure-rc))))))
      (exit reconfigure-rc))))
(testsymb 'main)

(module-evaluated)
