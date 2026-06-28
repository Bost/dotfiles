(define-module (scm-bin terminal-benchmark)
  #:use-module (dotf utils)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ terminal-benchmark) -s
!#

cd $dotf
./guix/home/common/scm-bin/terminal-benchmark.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define (getenv/default name fallback)
  (or (getenv name) fallback))

(define home-directory
  (getenv/default "HOME" "."))

(define cache-directory
  (string-append
   (getenv/default "XDG_CACHE_HOME"
                   (string-append home-directory "/.cache"))
   "/terminal-benchmark"))

(define output-file
  (string-append cache-directory "/eza-like-large-output.ansi"))

(define line-count
  (let ((parsed (string->number (getenv/default "LINE_COUNT" "100000"))))
    (if (and parsed (integer? parsed) (> parsed 0))
        parsed
        100000)))

(define escape
  (string (integer->char 27)))

(define (spaces count)
  (make-string (max 0 count) #\space))

(define (zeroes count)
  (make-string (max 0 count) #\0))

(define (pad-left width text)
  (string-append (spaces (- width (string-length text))) text))

(define (pad-right width text)
  (string-append text (spaces (- width (string-length text)))))

(define (zero-pad width number)
  (let ((text (number->string number)))
    (string-append (zeroes (- width (string-length text))) text)))

(define (ansi code text)
  (string-append escape "[" code "m" text escape "[0m"))

(define (write-colored-field port code text)
  (display (ansi code text) port)
  (display " " port))

(define (write-output-line port i)
  (let* ((inode (+ 1000000 i))
         (size (modulo (* i 7919) 100000000))
         (link-count (+ (modulo i 7) 1))
         (day (+ (modulo i 28) 1))
         (hour (modulo i 24))
         (minute (modulo i 60))
         (second (modulo (* i 7) 60))
         (timestamp
          (string-append
           "2026-01-" (zero-pad 2 day)
           " " (zero-pad 2 hour)
           ":" (zero-pad 2 minute)
           ":" (zero-pad 2 second)
           ".000000000 +0100"))
         (name
          (string-append
           "fake-guix-store-item-"
           (zero-pad 6 i)
           "-package-name-with-a-fairly-long-version-string-1.2.3")))

    ;; ANSI-colored, eza-ish long-listing line.
    (write-colored-field port "38;5;244" (pad-left 8 (number->string inode)))
    (write-colored-field port "38;5;33"  (pad-right 10 "-rw-r--r--"))
    (write-colored-field port "38;5;70"  (pad-left 2 (number->string link-count)))
    (write-colored-field port "38;5;136" (pad-right 8 "root"))
    (write-colored-field port "38;5;136" (pad-right 8 "root"))
    (write-colored-field port "38;5;208" (pad-left 10 (number->string size)))
    (write-colored-field port "38;5;244" timestamp)
    (display (ansi "1;38;5;39" name) port)
    (newline port)))

(define (make-output-file)
  (mkdir-p cache-directory)
  (call-with-output-file output-file
    (lambda (port)
      (let loop ((i 1))
        (when (<= i line-count)
          (write-output-line port i)
          (loop (+ i 1)))))))

(define (file-size path)
  (stat:size (stat path)))

(define (copy-file-to target)
  (call-with-input-file output-file
    (lambda (input)
      (call-with-output-file target
        (lambda (output)
          (let loop ()
            (let ((chunk (get-bytevector-n input 65536)))
              (unless (eof-object? chunk)
                (put-bytevector output chunk)
                (loop)))))))))

(define (elapsed-seconds thunk)
  (let ((start (get-internal-real-time)))
    (thunk)
    (exact->inexact
     (/ (- (get-internal-real-time) start)
        internal-time-units-per-second))))

(define (report-benchmark)
  (unless (file-exists? output-file)
    (make-output-file))

  (let* ((bytes (file-size output-file))
         (mib (/ bytes 1024.0 1024.0)))

    ;; Warm the file into the page cache, so the terminal is the main variable.
    (copy-file-to "/dev/null")

    (let* ((tty-seconds
            (elapsed-seconds
             (lambda ()
               (copy-file-to "/dev/tty"))))
           (null-seconds
            (elapsed-seconds
             (lambda ()
               (copy-file-to "/dev/null"))))
           (terminal-seconds (- tty-seconds null-seconds)))

      (newline)
      (format #t "file:                  ~a~%" output-file)
      (format #t "bytes:                 ~d~%" bytes)
      (format #t "MiB:                   ~,2f~%" mib)
      (format #t "cat > /dev/tty:        ~,3f s~%" tty-seconds)
      (format #t "cat > /dev/null:       ~,3f s~%" null-seconds)
      (format #t "approx terminal cost:  ~,3f s~%" terminal-seconds)

      (when (> terminal-seconds 0)
        (format #t
                "terminal throughput:   ~,2f MiB/s~%"
                (/ mib terminal-seconds))))))

(define (print-usage program)
  (format #t "Usage:~%")
  (format #t "  ~a --init     # create stable benchmark file once~%" program)
  (format #t "  ~a --bench    # render it and measure this terminal~%" program)
  (format #t "  ~a --reinit   # recreate it~%" program)
  (newline)
  (format #t "Optional:~%")
  (format #t "  LINE_COUNT=200000 ~a --reinit~%" program))

(define-public (main args)
  (let ((program (car args))
        (mode (match (cdr args)
                (() "--bench")
                ((first . _) first))))

    (cond
     ((string=? mode "--init")
      (make-output-file)
      (format #t "Created: ~a~%" output-file)
      (format #t "Bytes:   ~d~%" (file-size output-file)))

     ((string=? mode "--reinit")
      (when (file-exists? output-file)
        (delete-file output-file))
      (make-output-file)
      (format #t "Recreated: ~a~%" output-file)
      (format #t "Bytes:     ~d~%" (file-size output-file)))

     ((string=? mode "--bench")
      (report-benchmark))

     (else
      (print-usage program)
      (exit 2)))))

(module-evaluated)

