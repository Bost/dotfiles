(define-module (dotf tests)
  #:use-module (dotf utils)
  #:use-module (srfi srfi-1)      ; list-processing procedures
  #:use-module (guix build utils) ; find-files
  )

(define-syntax do-test
  (lambda (stx)
    (syntax-case stx ()
      [(do-test macro-name symbol arg ...)
       (begin
         ;; (when #t
         ;;   (format #t "[stx-c1] #'do-test    : ~a\n" #'do-test)
         ;;   (format #t "[stx-c1] #'macro-name : ~a\n" #'macro-name)
         ;;   (format #t "[stx-c1] #'symbol     : ~a\n" #'symbol)
         ;;   (format #t "[stx-c1] #'(arg ...)  : ~a\n" #'(arg ...)))

         #`((comp
             (lambda (function)
               ;; need to specify (ice-9 exceptions) for `guard' to avoid warnings
               ;; because of `error?' being defined in both (ice-9 exceptions) and
               ;; (rnrs conditions)
               (when ((@(ice-9 exceptions) guard)
                      ;; Break out in case of an error occured during the execution of
                      ;; `(apply function arg ...)' or `(function arg ...)'.
                      (condition [else
                                  (begin
                                    ;; (format #t "[stx-c1 guard-else] condition :\n~a\n" condition)
                                    #f)
                                  ])
                      (begin
                        ;; (when #t
                        ;;   ;; (format #t "[stx-c1 guard-body] macro-name      : ~a\n" macro-name)
                        ;;   (format #t "[stx-c1 guard-body] function        : ~a\n" function)
                        ;;   (format #t "[stx-c1 guard-body] symbol          : ~a\n" symbol))

                        (cond
                         [(eq? macro-name 'show-type-of-expression)
                          (begin
                            ;; (format #t "[stx-c1 guard-body c1]\n")
                            (function arg ...))]
                         [(eq? macro-name 'show-type-of-equality)
                          (begin
                            ;; (format #t "[stx-c1 guard-body c2]\n")
                            (apply function arg ...))]

                         [else
                          (begin
                            ;; (format #t "[stx-c1 guard-body else] Invalid syntax\n")
                            (syntax-violation
                             'macro-name
                             "[stx-c1 guard-body cond-else] Invalid syntax"
                             #'stx))])
                        ))
                 ;; The following `cond' is executed only if no guarded condition was
                 ;; triggered during the execution of `(function arg ...)', or if a
                 ;; guarded condition returned some true-value
                 (cond
                  [(and (list? symbol)
                        ;; error? can be from (ice-9 exceptions) or (rnrs conditions)
                        (not (equal? 'error? (last symbol))))
                   (begin
                     ;; (format #t "cond->(caddr symbol) : #t\n")
                     ;; (format #t "[stx-c1 cond-1] symbol : ~a\n" symbol)
                     (caddr symbol))]
                  [else
                   (begin
                     ;; (format #t "[stx-c1 else] symbol : ~a\n" symbol)
                     symbol)])
                 ))
             ;; (lambda (p) (format #t "1: ~a\n" p) p)
             (lambda (symb) (eval symb (interaction-environment)))
             ;; (lambda (p) (format #t "0: ~a\n" p) p)
             )
            symbol))]
      [else
       (begin
         ;; (format #t "[stx-else]\n")
         (syntax-violation 'do-test "[stx-celse] Invalid syntax" stx))]
      )))

;;; ### BEG: from /home/bost/dev/guile/module/ice-9/boot-9.scm
(define (list-of pred l)
  (or (null? l)
      (and (pair? l) (pred (car l)) (list-of pred (cdr l)))))

(define (valid-import? x)
  (list? x))

(define (valid-export? x)
  (or (symbol? x) (and (pair? x) (symbol? (car x)) (symbol? (cdr x)))))

(define (valid-autoload? x)
  (and (pair? x) (list-of symbol? (car x)) (list-of symbol? (cdr x))))

;;; ### END: from /home/bost/dev/guile/module/ice-9/boot-9.scm

(define-public (test-type single-argument)
  "See predicates https://en.wikipedia.org/wiki/Scheme_(programming_language)

Type Testing Predicates.
(tt (call-with-input-string \"  (+ x y)\" read-syntax)) ; => (syntax?)
(tt '())     ; => (list? null?)
(tt \"a\")   ; => (string?)
(tt 1)       ; => (number? complex? real? integer? rational? positive? odd?)
(tt (+ 1 2)) ; => (number? complex? real? integer? rational? positive? odd?)
(tt (* 3-8i 2.3+0.3i)) ; => (complex? number?)
(tt #\\space)          ; => (char-whitespace?)
(tt #\\a)              ; => (char-alphabetic?)
(tt #\\1)              ; => (char-numeric?)
(tt (gexp 42))         ; => (gexp?)
(tt (make-error))      ; => (record? exception? error?)
(tt (make-exception))  ; => (record? exception?)
(tt (/ 0.0 0.0))       ; => (number? complex? real? nan?)
(tt '(a b . c))        ; => (pair? nonempty-dotted-list?)
(tt '(a b c))          ; => (list? pair? proper-list?)
(tt '())               ; => (list? proper-list? null-list? not-pair? null?)

(tt (let ((x '(1 2 3))) (set-cdr! (cddr x) x) x))
; => (pair? circular-list?)

(tt (sqrt -1.0))       ; => (number? complex?)
(nan? (sqrt -1.0))            ; => Wrong type argument in position 1: 0.0+1.0i

(tt (make-exception ((@(ice-9 exceptions) make-error))))
; => (record? exception? (@ (ice-9 exceptions) error?) condition?)

(tt (make-exception ((@(rnrs conditions) make-error))))
; => (record? exception? (@ (rnrs conditions) error?) (@ (ice-9 exceptions) error?) condition?)

(tt (macroexpand '(define foo 42))) ; => (struct?)
"
  ((comp
    (partial remove unspecified?)
    (partial map (lambda (symbol) (do-test 'show-type-of-expression
                                           symbol single-argument))))
   (list
    'unspecified?
    'boolean?
    '(@(dotf utils) true?)
    '(@(dotf utils) false?)
    'port?
    'string?
    'symbol?
    'list?
    '(@(dotf utils) plist?)
    'pair?
    ;;
    '(@(srfi srfi-1) proper-list?)
    '(@(srfi srfi-1) circular-list?)
    ;; SRFI-1's dotted-list? treats any finite list whose final cdr is not '()
    ;; as a dotted (improper) list — and it allows the degenerate case with zero
    ;; pairs.
    ;; '(@(srfi srfi-1) dotted-list?)
    '(@(dotf utils) nonempty-dotted-list?)
    ;;
    '(@(srfi srfi-1) null-list?)
    '(@(srfi srfi-1) not-pair?)
    ;;
    'vector?
    'procedure?
    'record?
    'struct?
    'hash-table?
    ;;
    'number?
    'complex?
    'real?
    'integer?
    'rational?
    'positive?
    'negative?
    'odd?
    'even?
    'zero?
    ;; NaN - symbol to indicate that a mathematical operation could not produce
    ;; a meaningful result
    'nan?
    ;;
    '(@(system syntax internal) syntax?)
    'identifier?   ;; #t if syntax-object is an identifier, or #f otherwise.
    '(@(guix gexp) gexp?)
    'char?
    'null?
    'parameter? ;; ? is this for macros ?
    'eof-object?
    'char-alphabetic?
    'char-numeric?
    'char-whitespace?
    ;;
    'exception?
    '(@(rnrs conditions) error?)
    '(@(ice-9 exceptions) error?)
    ;;
    ;; Conditions are records of a subtype of the &condition record type, which
    ;; is neither sealed nor opaque. See R6RS Records.
    '(@(rnrs conditions) condition?)
    '(@(rnrs conditions) violation?)
    ;;
    '(@(language tree-il) void?)
    '(@(language tree-il) const?)
    '(@(language tree-il) lexical-ref?)
    '(@(language tree-il) lexical-set?)
    '(@(language tree-il) module-ref?)
    '(@(language tree-il) module-set?)
    '(@(language tree-il) toplevel-ref?)
    '(@(language tree-il) toplevel-set?)
    '(@(language tree-il) toplevel-define?)
    '(@(language tree-il) conditional?)
    '(@(language tree-il) call?)
    '(@(language tree-il) primcall?)
    '(@(language tree-il) seq?)
    '(@(language tree-il) lambda?)
    '(@(language tree-il) lambda-case?)
    '(@(language tree-il) let?)
    '(@(language tree-il) letrec?)
    '(@(language tree-il) fix?)
    '(@(language tree-il) let-values?)
    '(@(language tree-il) prompt?)
    '(@(language tree-il) abort?)
    ;;
    '(@(gnu packages) cache-is-authoritative?)
    ;;
    '(@(gnu services) ambiguous-target-service-error?)
    '(@(gnu services) for-home?)
    '(@(gnu services) linux-builder-configuration?)
    '(@(gnu services) missing-target-service-error?)
    '(@(gnu services) missing-value-service-error?)
    '(@(gnu services) service-error?)
    '(@(gnu services) service-extension?)
    '(@(gnu services) service-type?)
    '(@(gnu services) service?)
    ;;
    '(@(gnu home) home-environment?)
    ;;
    '(@(gnu system) operating-system?)
    '(@(gnu system) boot-parameters?)
    ;;
    '(@(gnu bootloader) bootloader-configuration-device-tree-support?)
    '(@(gnu bootloader) bootloader-configuration?)
    '(@(gnu bootloader) bootloader?)
    '(@(gnu bootloader) menu-entry?)
    ;;
    '(@(gnu services configuration) configuration-error?)
    '(@(gnu services configuration) empty-serializer?)
    '(@(gnu services configuration) maybe-value-set?)
    '(@(gnu services configuration) list-of-packages?)
    '(@(gnu services configuration) list-of-strings?)
    '(@(gnu services configuration) list-of-symbols?)
    '(@(gnu services configuration) alist?)
    '(@(gnu services configuration) text-config?)
    ;;
    '(@(gnu services base) host?)
    '(@(gnu services base) static-networking?)
    '(@(gnu services base) network-address?)
    '(@(gnu services base) network-address-ipv6?)
    '(@(gnu services base) network-link?)
    '(@(gnu services base) network-route?)
    '(@(gnu services base) network-route-ipv6?)
    ;;
    '(@(guix colors) color-output?)
    ;;
    '(@(guix grafts) graft?)
    ;;
    '(@(guix narinfo) narinfo?)
    '(@(guix narinfo) valid-narinfo?)
    '(@(guix narinfo) equivalent-narinfo?)
    ;;
    '(@(guix derivations) derivation?)
    '(@(guix derivations) derivation-output?)
    '(@(guix derivations) derivation-output-recursive?)
    '(@(guix derivations) derivation-input?)
    '(@(guix derivations) valid-derivation-input?)
    '(@(guix derivations) derivation-error?)
    '(@(guix derivations) derivation-missing-output-error?)
    '(@(guix derivations) derivation-missing-output)
    '(@(guix derivations) fixed-output-derivation?)
    '(@(guix derivations) offloadable-derivation?)
    '(@(guix derivations) substitutable-derivation?)
    '(@(guix derivations) file-search-error?)
    ;;
    '(@(guix store) substitutable?)
    '(@(guix store) path-info?)
    '(@(guix store) store-connection?)
    '(@(guix store) store-error?)
    '(@(guix store) store-connection-error?)
    '(@(guix store) store-protocol-error?)
    '(@(guix store) grafting?)
    '(@(guix store) store-path?)
    '(@(guix store) direct-store-path?)
    '(@(guix store) derivation-path?)
    ;;
    '(@(guix profiles) profile-error?)
    '(@(guix profiles) profile-not-found-error?)
    '(@(guix profiles) profile-collision-error?)
    '(@(guix profiles) missing-generation-error?)
    '(@(guix profiles) manifest-entry?)
    '(@(guix profiles) manifest-entry=?)
    '(@(guix profiles) manifest-pattern?)
    '(@(guix profiles) manifest-installed?)
    '(@(guix profiles) manifest-transaction?)
    '(@(guix profiles) manifest-transaction-null?)
    '(@(guix profiles) manifest-transaction-removal-candidate?)
    '(@(guix profiles) profile?)
    '(@(guix profiles) profile-locales?)
    '(@(guix profiles) profile-allow-collisions?)
    '(@(guix profiles) profile-relative-symlinks?)
    ;;
    '(@(guix gexp) gexp?)
    '(@(guix gexp) gexp-input?)
    '(@(guix gexp) gexp-input-native?)
    '(@(guix gexp) local-file?)
    '(@(guix gexp) local-file-recursive?)
    '(@(guix gexp) local-file-select?)
    '(@(guix gexp) plain-file?)
    '(@(guix gexp) computed-file?)
    '(@(guix gexp) program-file?)
    '(@(guix gexp) scheme-file?)
    '(@(guix gexp) file-append?)
    '(@(guix gexp) raw-derivation-file?)
    '(@(guix gexp) parameterized?)
    '(@(guix gexp) delayed-object?)
    '(@(guix gexp) lowered-gexp?)
    '(@(guix gexp) gexp-compiler?)
    '(@(guix gexp) file-like?)
    '(@(guix gexp) gexp-error?)
    '(@(guix gexp) gexp-input-error?)
    ;;
    '(@(guix channels) channel?)
    '(@(guix channels) channel-introduction?)
    '(@(guix channels) guix-channel?)
    '(@(guix channels) channel-instance?)
    '(@(guix channels) channel-news-entry?)
    ;;
    '(@(guix packages) content-hash?)
    '(@(guix packages) hidden-package?)
    '(@(guix packages) origin?)
    '(@(guix packages) package-cross-build-system-error?)
    '(@(guix packages) package-cyclic-dependency-error?)
    '(@(guix packages) package-error?)
    '(@(guix packages) package-input-error?)
    '(@(guix packages) package-license-error?)
    '(@(guix packages) package-location<?)
    '(@(guix packages) package-unsupported-target-error?)
    '(@(guix packages) package?)
    '(@(guix packages) supported-package?)
    '(@(guix packages) unsupported-cross-compilation-target-error?)
    ;;
    '(@(guix store roots) user-owned?)
    ;;
    '(@(guix tests) canonical-file?)
    ;;
    '(@(guix sets) set?)
    ;;
    '(@(guix quirks) patch?)
    '(@(guix quirks) applicable-patch?)
    ;;
    '(@(guix monads) monad?)
    ;;
    '(@(guix modules) guix-module-name?)
    )))
(define-public tt test-type)

(define-public (test-equality . args)
  "Equality and Comparison Predicates. Variadic (i.e. infinite arity)
(te)           ; => <all predicates>
(te 1)         ; =>
(te 1 2)       ; => ()
(te 1 1)       ; => (= eq? eqv? equal?)
(te 1 1 'x)    ; => ()
(te \"1\" \"1\")   ; => (string=? string-ci=? eq? eqv? equal?)
(te 1 1 1)     ; => (= eq? eqv? equal?)

(te (list 1) (list 1))      ; => (list=eq? list=eqv? list=equal? equal?)
(te (list 1) (list 1) '(1)) ; => (list=eq? list=eqv? list=equal? equal?)

(te (list 1) (list 2))    ; =>
(te (list 1) (list 2) 'x) ; => ()
(te (list 1) (list 1) 'x) ; => ()
"
  ((comp
    (partial remove unspecified?)
    (partial map (lambda (symbol) (do-test 'show-type-of-equality symbol args))))
   (list
    '=
    '<=
    '>=
    'string=? ;; returns #t only if both parameters are strings
    'string-ci=?
    'char=?
    'char-ci=?

    '(@(dotf utils) list=eq?)
    '(@(dotf utils) list=eqv?)
    '(@(dotf utils) list=equal?)

    'lset=  ; from srfi-1
    'lset<= ; from srfi-1

    'eq?
    'eqv?
    'equal?

    '(@(dotf utils) some-true?)
    '(@(dotf utils) every-true?)
    '((@(dotf utils) partial) (@(dotf utils) not-every?) (@(dotf utils) true?))
    '((@(dotf utils) partial) (@(dotf utils) not-any?) (@(dotf utils) true?))

    ;; Tree Intermediate Language
    '(@(language tree-il) tree-il=?)

    '(@(guix tests) file=?)
    )))
(define-public te test-equality)
