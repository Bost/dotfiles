(define-module (scm-bin sgxsr)
  #:use-module (bost common utils) ; str, module-name-for-logging, etc.
  #:use-module (dotf fs-utils)     ; dtfg
  #:use-module (guix build utils)  ; which
  #:use-module (srfi srfi-1)       ; filter-map
  #:use-module (srfi srfi-37)      ; args-fold CLI option parsing
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

(define prm-args-pull         "--args-pull")
(define prm-dry-run           "--dry-run")
(define prm-dry-run-short     "-n")

(define prm-no-pull           "--no-pull")
(define prm-args-system       "--args-system")
(define prm-args-pull-short   "-P")
(define prm-args-system-short "-S")

(define sgxsr-args
  (list
   prm-args-pull
   prm-no-pull
   prm-args-system
   prm-args-pull-short
   prm-args-system-short
   ))

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

(define* (run-guix-command args #:key (dry-run #f))
  "Run ARGS without whitespace splitting and return a shell-style exit code.
With DRY-RUN, execute Guix with --dry-run to calculate planned changes. An
exec-system* dry run returns zero; a signal termination returns 128 plus the
signal."
  (let ((status
         (apply
          exec-system* #:verbose #t #:split-whitespace #f
          (if dry-run
              (append args
                      ;; Print-only alternative: prevent exec-system* from
                      ;; starting a process.
                      ;; '("--gx-dry-run")
                      '("--dry-run"))
              args))))
    ;; exec-system* returns the argv list rather than a wait status for dry
    ;; runs.
    (if (list? status) 0 (wait-status->exit-code status))))

(def (parse-command-args args)
  "Collect repeated -P and -S values unchanged, preserving their order.
Reject unknown options, operands, missing values, and pull with --no-pull."
  (define (fail message . values)
    (apply format (current-error-port)
           (string-append "sgxsr: " message "\n") values)
    (exit 2))
  (define (option-name name)
    (if (char? name) (string #\- name) (string-append "--" name)))
  (define (prepare args)
    ;; Attached long-option values accept leading hyphens in Guile's SRFI-37.
    ;; Preserve the original option names and never rewrite forwarded values.
    (cond ((null? args) '())
          ((member (car args) (list prm-args-pull prm-args-system))
           (when (null? (cdr args))
             (fail "~a requires an argument value" (car args)))
           (cons (string-append (car args) "=" (cadr args))
                 (prepare (cddr args))))
          ((member (car args) (list prm-args-pull-short prm-args-system-short))
           (when (null? (cdr args))
             (fail "~a requires an argument value" (car args)))
           (cons (car args) (cons (cadr args) (prepare (cddr args)))))
          (else (cons (car args) (prepare (cdr args))))))
  (define (collect key)
    (lambda (option name value options)
      (unless (and value (not (string-null? value)))
        (fail "~a requires one nonempty argument value" (option-name name)))
      (when (member value sgxsr-args)
        (fail "~a requires an argument value, got sgxsr option '~a'"
              (option-name name) value))
      (acons key value options)))
  (let* ((option-spec
          (list (option (list (string-ref prm-args-pull-short 1)
                              (substring prm-args-pull 2))
                        #t #f (collect 'args-pull))
                (option (list (string-ref prm-args-system-short 1)
                              (substring prm-args-system 2))
                        #t #f (collect 'args-system))
                (option (list (substring prm-no-pull 2)) #f #f
                        (lambda (option name value options)
                          (acons 'no-pull #t options)))
                (option (list (substring prm-dry-run 2)
                              (string-ref prm-dry-run-short 1)) #f #f
                        (lambda (option name value options)
                          (acons 'dry-run #t options)))))
         (options
          (catch 'misc-error
            (lambda ()
              (args-fold (prepare args)
                         option-spec
                         (lambda (option name value options)
                           (fail "unrecognized option '~a'" (option-name name)))
                         (lambda (operand options)
                           (fail "argument '~a' requires ~a (-P) or ~a (-S)"
                                 operand prm-args-pull prm-args-system))
                         '()))
            (lambda (key procedure message arguments . rest)
              (fail "~a" (apply format #f message arguments)))))
         (values-for
          (lambda (key)
            (filter-map (lambda (entry)
                          (and (eq? (car entry) key) (cdr entry)))
                        (reverse options))))
         (val-args-pull (values-for 'args-pull))
         (val-args-system (values-for 'args-system))
         (val-no-pull (assoc-ref options 'no-pull))
         (val-dry-run (assoc-ref options 'dry-run)))
    (trc "option-spec:" option-spec)
    (trc "options:" options)
    (trc "val-args-pull:" val-args-pull)
    (trc "val-args-system:" val-args-system)
    (trc "val-no-pull:" val-no-pull)
    (trc "val-dry-run:" val-dry-run)
    (when (and val-no-pull (pair? val-args-pull))
      (fail "~a cannot be combined with ~a (-P)" prm-no-pull prm-args-pull))
    `((args-pull . ,val-args-pull)
      (args-system . ,val-args-system)
      (no-pull . ,val-no-pull)
      (dry-run . ,val-dry-run))))

(define*-public (main #:rest args)
  "Pull system channels, reconfigure the Guix system, roll back to
home-channels.

`--args-pull' or `-P' passes one argument unchanged to the first `guix pull'.
`--args-system' or `-S' passes one argument unchanged to `guix system'.
Repeat the wrapper option for every forwarded argument, including values
of Guix options.  Supply the Guix option's original -- or - prefix.
Quoted spaces are preserved within a single argument; values are never split.
Unknown wrapper options, unmarked arguments, and missing values are rejected
before any command runs.  The rollback receives no extra arguments.

Top-level `--dry-run' or `-n' runs pull and system with Guix's --dry-run
so Guix calculates planned changes.  Rollback and notifications are skipped.
Repeating it is harmless.
A dry-run value forwarded through -P or -S only affects that Guix command;
it does not enable sgxsr's dry run.

Examples:
  sgxsr --dry-run -P --switch-generation=1919 -S --allow-downgrades
  sgxsr -n --no-pull -S --allow-downgrades
  sgxsr -P --allow-downgrades -S --allow-downgrades -S --dry-run
  sgxsr --no-pull -S --verbosity=3
  sgxsr --args-pull=--allow-downgrades --args-system=--dry-run
  sgxsr -S --load-path=/path
  sgxsr -P -L -P '/path containing spaces'
  sgxsr -S -L -S '/path containing spaces' -S -v

`--no-pull' skips both pull and rollback and cannot be combined with
`--args-pull' or `-P'.

TODO Implement `sgxsr --equal-to-home-channels'
1. comment out existing in syst-channels.scm
2. add the channels from home-channels.scm
3. prepend timestamp
"
 (let* ((command-args (parse-command-args (cdr args)))
        (no-pull (assoc-ref command-args 'no-pull))
        (dry-run (assoc-ref command-args 'dry-run))
        (pull-args (assoc-ref command-args 'args-pull))
        (system-args (assoc-ref command-args 'args-system))
        (config (str dtfg "/systems/syst-" (gethostname) ".scm")))

    (unless (file-exists? config)
      (format (current-error-port)
              "No system config for host '~a': ~a\n"
              (gethostname) config)
      (exit 1))

    (unless no-pull
      ;; No automatic --allow-downgrades for system channels.
      (let ((rc (run-guix-command
                        `("guix" "pull"
                          "--unsafe-channel-evaluation"
                          ,(string-append "--load-path=" common-lp)
                          ,(string-append "--channels=" channels-scm)
                          ,@pull-args
                          ) #:dry-run dry-run)))
        (unless (zero? rc) (exit rc)))

      (unless dry-run
        (notify "Done" "`guix pull` finished")))

    ;; The channels for system and home configurations may differ.  Roll back
    ;; the pull above to reactivate home-channels.  This won't behave correctly
    ;; if `guix pull' with system-channels was called more than once
    ;; consecutively, but it's faster than pulling the home-channels.
    ;; If the roll-back fails, use `gxp'.
    ;;
    ;; dynamic-wind guarantees the roll-back runs even if reconfigure fails,
    ;; mirroring the `trap ... EXIT' in the bash version.
    (let ((reconfigure-rc 0)
          (rollback-rc 0))
      (dynamic-wind
        (lambda () #f)
        (lambda ()
          ;; No sudo --login: the guix pull above already updated the current
          ;; environment; --login would source root's profile and pick up
          ;; root's older Guix generation instead.
          (set! reconfigure-rc
                (run-guix-command
                        `("sudo" "guix" "system"
                          ,@system-args
                          "--verbosity=3" "--fallback"
                          ,(string-append "--load-path=" common-lp)
                          ,(string-append "--load-path=" systems-common-lp)
                          "reconfigure" ,config
                          ) #:dry-run dry-run)))
        (lambda ()
          (unless (or no-pull dry-run)
            (set! rollback-rc
                  (run-guix-command '("guix" "pull" "--roll-back")
                                    #:dry-run dry-run)))))
      ;; Exit after leaving dynamic-wind, never from its cleanup handler.
      (exit (if (zero? reconfigure-rc) rollback-rc reconfigure-rc)))))
(testsymb 'main)

(module-evaluated)
