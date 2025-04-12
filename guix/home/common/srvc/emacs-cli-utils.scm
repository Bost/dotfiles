(define-module (srvc emacs-cli-utils)
  #:use-module (utils)
  #:use-module (tests)
  #:use-module (settings)
  #:use-module (fs-utils)

  ;; take remove delete-duplicates append-map etc.
  #:use-module (srfi srfi-1)
  ;; #:use-module (ice-9 pretty-print)
  )

(define m (module-name-for-logging))
(evaluating-module)

;; <util> is the name of the procedure from (emacs-common)
(define fun->util--mapping
  (list
   ;; set-editable
   (cons 'launcher 'create-emacs-launcher)
   (cons 'pkill    'pkill-server)
   (cons 'editable 'set-config-editable)
   ))

(define-public emacs-utils
  ((comp
    (partial map car))
   fun->util--mapping))

#|
(define-public emacs-profiles
  (append (map (comp string->symbol car) profile->branch-kw)
          (list 'guix 'install-packages)))
|#

(define-public emacs-profiles (list 'crafted 'develop 'spguimacs))

;;; (create-def--emacs-<util>-<profile> 'my-util 'my-profile)
;;;   => my-util-my-profile => "emacs-my-util-my-profile"
(define-syntax create-def--emacs-<util>-<profile>
  (syntax-rules ()
    ((_  sym-util sym-profile)
     (let* [(m (format #f "~a [create-def--emacs-<util>-<profile>]" m))

            (str-util (symbol->string sym-util))
            (str-profile (symbol->string sym-profile))

            (str-def-name (format #f "~a-~a" str-util str-profile))
            (sym-def-name (string->symbol str-def-name))

            (str-def-body (format #f "emacs-~a-~a" str-util str-profile))]
       ;; Scheme Procedure: define! sym value Define SYM to be VALUE in the
       ;; current module. Returns the variable itself. Note that this is a
       ;; procedure, not a macro.
       ;; (format #t "~s\n" `(define! ,sym-def-name ,str-def-body))
       (define! sym-def-name str-def-body)

       ;; module-export! is a procedure
       ;; see `define-syntax-rule (export name ...)' in ~/dev/guile/module/ice-9/boot-9.scm
       (module-export! (current-module) (list sym-def-name))))))

(define-syntax define-utils
  (syntax-rules ()
    ((_ utils profiles)
     (map (partial apply (lambda (u t)
                           (create-def--emacs-<util>-<profile> u t)))
          (cartesian utils profiles)))))

(define-utils emacs-utils emacs-profiles)

(define-syntax create-util-lists
  (syntax-rules ()
    ((_ utils profiles)
     (begin
       (define-syntax create-util-lst
         (syntax-rules ()
           ((_ util profiles)
            (let [(m (format #f "~a [create-util-lst]" m))
                  (sym-def-name (string->symbol (format #f "~a-lst" util)))
                  (str-def-body (map (comp
                                 (partial format #f "emacs-~a-~a" util)
                                 symbol->string)
                                profiles))]
              ;; (format #t "~s\n" `(define! ,sym-def-name ,str-def-body))
              (define! sym-def-name str-def-body)
              (module-export! (current-module) (list sym-def-name))))))

       (map (lambda (util)
              (create-util-lst util profiles))
            utils)))))

(create-util-lists emacs-utils emacs-profiles)

(define-public editable-profiles "emacs-editable-profiles") ;; ~/.emacs-profiles.el
(define-public editable-lst (append editable-lst (list editable-profiles)))
(testsymb-trace 'editable-lst)

(module-evaluated #t)
