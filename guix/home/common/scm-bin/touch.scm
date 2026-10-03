(define-module (scm-bin touch)
;;; All used modules must be present in (@(services cli-utils) common-modules)
  #:use-module (bost common utils) ; comp, partial, logging helpers
  #:use-module (srfi srfi-1)       ; remove
  #:use-module (ice-9 optargs)     ; define*-public
  #:use-module (ice-9 getopt-long) ; -p/--parents -v/--verbose -h/--help parsing
  )

#|

#!/usr/bin/env -S guix repl --
!#

cd $dotf
# not '(apply main (command-line))'
echo -e "\n(main (command-line))" >> ./guix/home/common/scm-bin/touch.scm
./guix/home/common/scm-bin/touch.scm -pv /tmp/touch-test/a/b/c.txt

|#

(define m (module-name-for-logging))
(evaluating-module)

(define usage
  "Usage: touch [OPTION]... FILE...
Create each FILE if it doesn't exist, otherwise update its access and
modification times to now.

  -p, --parents   create missing parent directories, like `mkdir -p'
  -v, --verbose   say what was created / touched
  -h, --help      display this help and exit
")

(define (mkdir-p dir)
  "\"a/b/c\" -> (\"a\" \"a/b\" \"a/b/c\") i.e. the directories actually created,
outermost first. Pure Guile (no (guix build utils)), so this stays a leaf module.
Throws a 'system-error if a path component exists but isn't a directory."
  (let loop [(dir dir) (created '())]
    (if (file-exists? dir)
        (if (eq? 'directory (stat:type (stat dir)))
            created
            (throw 'system-error "mkdir-p" "~A: ~A"
                   (list (strerror ENOTDIR) dir) (list ENOTDIR)))
        (let [(created (loop (dirname dir) created))]
          (mkdir dir)
          (append created (list dir))))))

(define* (touch-file path #:key parents verbose)
  "Touch a single PATH. -> #t on success, #f on failure (reason already printed
on stderr)."
  (define (say fmt . args)
    (when verbose (apply format #t fmt args)))
  (catch 'system-error
    (lambda ()
      (let [(parent (dirname path))]
        (cond
         [(string-suffix? "/" path)
          (format (current-error-port)
                  "touch: '~a' ends with '/'; for a directory use `mkdir -p'\n"
                  path)
          #f]
         [(and (not parents) (not (file-exists? parent)))
          (format (current-error-port)
                  "touch: cannot touch '~a': directory '~a' doesn't exist (use -p to create it)\n"
                  path parent)
          #f]
         [else
          (when parents
            (map (partial say "created directory '~a'\n") (mkdir-p parent)))
          (if (file-exists? path)
              (begin
                (utime path)  ; no times given -> both set to now
                (say "touched '~a'\n" path))
              (begin
                (close-port (open path (logior O_WRONLY O_CREAT) #o666))
                (say "created file '~a'\n" path)))
          #t])))
    (lambda (key subr fmt fmt-args . rest)
      (format (current-error-port) "touch: cannot touch '~a': ~a\n"
              path (apply format #f fmt fmt-args))
      #f)))

(define*-public (touch #:key parents verbose #:rest args)
  "Touch every path in ARGS. -> list of the paths which failed, i.e. '() on
success.
Usage:
(touch #:parents #t #:verbose #t \"/tmp/touch-test/a/b/c.txt\" \"/tmp/x.txt\")"
  (let [(paths (filter string? args))]
    (remove (lambda (path)
              (touch-file path #:parents parents #:verbose verbose))
            paths)))
(testsymb 'touch)

(define-public (main args)
  "Usage:
(main (list \"<ignored>\" \"-p\" \"-v\" \"/tmp/touch-test/a/b/c.txt\"))
Exits with 1 if any FILE couldn't be touched."
  (let* [(option-spec '((parents (single-char #\p) (value #f))
                        (verbose (single-char #\v) (value #f))
                        (help    (single-char #\h) (value #f))))
         ;; `getopt-long' expects ARGS's car to be the program name, as
         ;; `command-line' would produce it -- it's skipped, not parsed.
         ;; Unknown options are reported by `getopt-long' itself, then exit 1.
         (options (getopt-long args option-spec))
         (paths (option-ref options '() '()))]
    (cond
     [(option-ref options 'help #f)
      (display usage)]
     [(null? paths)
      (format (current-error-port) "touch: missing file operand\n~a" usage)
      (exit 2)]
     [else
      ((comp
        (lambda (failed) (unless (null? failed) (exit 1)))
        (partial apply touch
                 #:parents (option-ref options 'parents #f)
                 #:verbose (option-ref options 'verbose #f)))
       paths)])))
(testsymb 'main)

(module-evaluated)
