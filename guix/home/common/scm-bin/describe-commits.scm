(define-module (scm-bin describe-commits)
  #:use-module (dotf utils)
  #:use-module (ice-9 optargs)            ; define*-public
  )

(define m (module-name-for-logging))
(evaluating-module)

;; Shared core for the guix(-system)-describe scripts.  A "channel" here is a
;; (NAME . COMMIT) pair, NAME a symbol; each front-end reduces its input to such
;; pairs, so car/cdr are the only accessors the formatting needs.

(define*-public (channel->keyword #:key args)
  "(NAME . COMMIT) -> \"#:NAME-commit\""
  ((comp
    (lambda (s) (string-append "#:" s "-commit"))
    symbol->string
    car)
   args))

(define-public (channel->entry pair)
  "(NAME . COMMIT) -> (\"#:NAME-commit\" . \"COMMIT\")"
  (cons (channel->keyword #:args pair) (cdr pair)))

(define*-public (entries->max-width #:key args)
  "entries -> width of the widest keyword (so the commit column lines up)"
  ((comp (partial apply max)
         (partial map (comp string-length car)))
   args))

(define-public (format-entry width entry)
  "(width entry) -> \" #:NAME-commit   \"COMMIT\"\" (leading space included)"
  (let ((kw (car entry)))
    (string-append " " kw
                   (make-string (- (1+ width) (string-length kw)) #\space)
                   "\"" (cdr entry) "\"")))

(define-public (commit-block date-string pairs)
  "DATE-STRING and ((NAME . COMMIT) ...) -> list of output lines: a
\" ;; DATE-STRING\" comment followed by the aligned #:NAME-commit block."
  (let* ((entries (map channel->entry pairs))
         (width   (entries->max-width #:args entries)))
    (cons (string-append " ;; " date-string)
          (map (partial format-entry width) entries))))

(module-evaluated)
