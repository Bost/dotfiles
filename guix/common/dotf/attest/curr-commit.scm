;;; Reuse Guix's CLI as the API (the Scheme modules are not really a stable
;;; public interface and keep moving around)

(define-module (dotf attest curr-commit)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 pretty-print)
  #:use-module (guix channels) ; profile-channels

  #:export
  (
   curr-commit
   guix-channel-url
   guix-channel-commit
   current-channels
   ))

(define (read-all port)
  (let loop ((acc '()))
    (let ((x (read port)))
      (if (eof-object? x) (reverse acc) (loop (cons x acc))))))

(define (current-channels)
  ;; returns whatever `(guix describe -f channels)` prints, as Scheme data
  (let* ((p (open-pipe* OPEN_READ "guix" "describe" "--format=channels"))
         (xs (read-all p)))
    (close-pipe p)
    ;; typically exactly one top-level expr, but keep it robust:
    (if (= (length xs) 1) (car xs) `(begin ,@xs))))

(define (current-channels-new)
  (let* ((profile (string-append (getenv "HOME") "/.config/guix/current"))
         (channels (profile-channels profile)))
    `(list ,@(map channel->code channels))))

(define* (guix-channel-commit #:key
                              (name 'guix)
                              (channels (current-channels)))
  "Return the commit of the (name NAME) channel from CHANNELS.
Examples:
(guix-channel-commit #:name 'guix #:channels (current-channels))
"
  ;; channels is usually:
  ;;   (list (channel (name 'guix) (url ...) (commit "...") ...) ...)
  ;; Do simple s-expression walk to find (name 'guix) and then (commit "...")
  ;; fields is ( (name ...) (url ...) (commit ...) ...)
  (define (assoc-kw key fields)
    (find (lambda (f) (and (pair? f) (eq? (car f) key))) fields))

  (define (channel-fields channel-form)
    ;; (channel <field> <field> ...)
    (cdr channel-form))

  (define (field-value field) (cadr field))

  (let* ((channels
          (cond
            ((and (pair? channels) (eq? (car channels) 'list))
             (cdr channels))
            (else
             (error "Unexpected format" channels))))
         (guix-chan
          (find (lambda (ch)

                  (and (pair? ch) (eq? (car ch) 'channel)
                       (let* ((fs (channel-fields ch))
                              (nm (assoc-kw 'name fs)))
                         (and nm (equal? (cadr (field-value nm))
                                         name)))))
                channels)))
    (unless guix-chan
      (error
       ;; ~y pretty-print
       (format #f "Could not find (name ~s) channel among:\n~y"
               name
               channels)))
    (let* ((fs (channel-fields guix-chan))
           (cm (assoc-kw 'commit fs)))
      (unless cm
        (error "Guix channel has no commit field?" guix-chan))
      (field-value cm))))

(define* (guix-channel-url #:key
                           (name 'guix)
                           (channels (current-channels)))
  "Return the URL of the (name NAME) channel from CHANNELS.
Examples:
(guix-channel-url #:name 'guix #:channels (current-channels))
"
  (define (assoc-kw key fields)
    (find (lambda (f) (and (pair? f) (eq? (car f) key))) fields))

  (define (channel-fields channel-form)
    (cdr channel-form))

  (define (field-value field) (cadr field))

  (let* ((channels
          (cond
            ((and (pair? channels) (eq? (car channels) 'list))
             (cdr channels))
            (else
             (error "Unexpected format" channels))))
         (guix-chan
          (find (lambda (ch)
                  (and (pair? ch) (eq? (car ch) 'channel)
                       (let* ((fs (channel-fields ch))
                              (nm (assoc-kw 'name fs)))
                         (and nm (equal? (cadr (field-value nm))
                                         name)))))
                channels)))
    (unless guix-chan
      (error
       (format #f "Could not find (name ~s) channel among:\n~y"
               name
               channels)))
    (let* ((fs (channel-fields guix-chan))
           (url (assoc-kw 'url fs)))
      (unless url
        (error "Guix channel has no url field?" guix-chan))
      (field-value url))))

;; Usage:
(define (curr-commit) (guix-channel-commit
                       #:name 'guix
                       #:channels (current-channels)))
;; (curr-commit)  ;; => "abcd1234..."

