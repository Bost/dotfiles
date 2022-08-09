(use-modules (ice-9 rdelim)
             (ice-9 regex)             #| string-match |#
             (ice-9 popen)
             (utils)
             #;(guix build utils) #| invoke |#
             (ice-9 match)
             (ice-9 pretty-print)
             (system syntax)
             )
#|

I have a syntax-object and I want to write it to a different file, formatted as described by the line and column information. Is it possible?
e.g. '(syntax-source (call-with-input-string "   (+ 1 2)" read-syntax))'

(define (read-all-syntax port)
  "Return a list of all lines from the PORT."
  (let loop ((res '())
             (str (read-syntax port))) ; from (ice-9 popen)
    (if (and str (not (eof-object? str)))
        (loop (append res (list str))
              (read-syntax port))
        res)))
(define gp "/home/bost/dev/guix-playground")
(define fs (format #f "~a/gnu/packages/e.scm" gp))
(syntax-sourcev )

(define stx (call-with-input-file fs read-all-syntax))
(define stx (call-with-input-string "   (+ 1 2)" read-syntax))
(define fn "/tmp/emacs-new.scm")

(with-output-to-file fn (lambda () (write stx)))
|#
(define (create-package base32 hash)
  "Create an emacs-next-<hash> package"
  (let* ((h 'emacs-next-) ; intern symbol
         (e (symbol-append h (string->symbol hash))))
    `(define-public ,e
       (package
         (inherit emacs)
         (name (symbol->string ,e))
         (source
          (origin
            (inherit (package-source emacs))
            (method url-fetch)
            (uri (format #f "file:///home/bost/dev/emacs-~a" hash))
            (sha256
             (base32 ,base32))
            (patches (remove (lambda (patch)
                               (member (basename patch)
                                       '("emacs-exec-path.patch"
                                         "emacs-ignore-empty-xim-styles.patch")))
                             (origin-patches (package-source emacs))))
            ))
         (native-inputs
          (modify-inputs (package-native-inputs emacs)
            (prepend autoconf)))))))

(define (process base32 hash dt)
  "Process datums"
  (filter (lambda (e)
            (match e
              (('define-public name v1 ...)
               (if (string-match "emacs-next" (symbol->string name))
                   (begin
                     (set! counter (1+ counter))
                     (create-package base32 hash))
                   `(define-public ,name ,v1 ...)))
              (_ e)))
          dt))

(define (read-all-syntax port)
  "Return a list of all lines from the PORT."
  (let loop ((res '())
             (str (read-syntax port))) ; from (ice-9 popen)
    (if (and str (not (eof-object? str)))
        (loop (append res (list str))
              (read-syntax port))
        res)))

(define counter 0)

(define (main args)
  (let* ((full-hash (cadr args))
         (hash (substring full-hash 0 10))
         (wd (string-append "/home/bost/dev/emacs-" hash))
         (gd "/home/bost/dev/emacs/.git")
         (gp "/home/bost/dev/guix-playground"))

    #;(exec (format #f "mkdir -p ~a" wd))
    #;(exec (format #f "git --git-dir=~a --work-tree=~a checkout ~a" gd wd hash))

    (let ((ret (exec (format #f "guix hash --serializer=nar -x ~a" wd))))
      (if (= 0 (car ret))
          (let* ((output (cdr ret))
                 (base32 (car output)))
            (let* (
                   (fs (format #f
                               "~a/gnu/packages/e.scm"
                               #;"~a/gnu/packages/emacs.scm" gp))
                   (fn "/tmp/emacs-new.scm")
                   (sy (call-with-input-file fs read-all-syntax))
                   (dt (syntax->datum sy))
                   (new-dt (process base32 hash dt))
                   )
              #;(format #t "define-public: ~a; total: ~a" counter (length dt))
              (format #t "sy: ~a\n" sy)
              (format #t "sy: ~a\n" (syntax-sourcev (car sy)))
              (call-with-output-file fn (lambda (port)
                                          (pretty-print new-dt port)
                                        #;(format #t "~a\n"
                                                sy
                                                #;new-dt)))
              #;
              (match-let* (((fn n vo) dt)
                           (r `(,fn fox ,(1+ vo))))
                (call-with-output-file fs (lambda (port)
                                            (format port "~a\n" r)))
                (with-output-to-file fs (lambda ()
                                          (format #t "~a\n~a\n" dt r))))
              )
            #;(begin
              (format #f "sed -i -e \"s|hashhash|~a|\" ~a/gnu/packages/emacs.scm" hash gp)
              (format #f "sed -i -e \"s|basebase|~a|\" ~a/gnu/packages/emacs.scm" base32 gp)
              (format #f "guix build --cores=20 --load-path=~a emacs-next-~a" gp hash)))
          (format #t "ERR: Command failed")))))

#;(eval-string "(+ 1 2)" (interaction-environment))
#;(eval '(+ 1 2) (interaction-environment))

#;(cdr (syntax->datum (call-with-input-string "(+ 1 2)" read-syntax)))

#;(eval (cadr (syntax->datum
             (call-with-input-string
                 "(foo (+ 1 2))"
               read-syntax)))
      (interaction-environment))
