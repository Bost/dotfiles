#lang rash

(displayln "=== Loading crep.rkt")

(require
 linea/line-macro
 (for-syntax racket/base syntax/parse))

(provide (all-defined-out))

(define-line-macro crl
  (lambda (stx)
    (syntax-parse stx
      [(_ arg) #'(cheat-grep `arg '(
                                    dev/cheat/cmds/emacs.org
                                    dev/cheat/cmds/vim.org
                                    dev/cheat/cmds/shells.org
                                    dev/cheat/cmds/linux.org
                                    dev/cheat/cmds/rest.org
                                    dev/cheat/cmds/findgrep.org
                                    dev/cheat/cmds/systemd.org
                                    dev/cheat/cmds/git.org
                                    dev/cheat/cmds/packaging.org
                                    ))])))

(define-line-macro cre
  ;; TODO combile cre with grep of .spacemacs
  (lambda (stx)
    (syntax-parse stx
      [(_ arg) #'(cheat-grep `arg '(
                                    dev/cheat/cmds/emacs.org
                                    ))])))

(define-line-macro crf
  (lambda (stx)
    (syntax-parse stx
      [(_ arg) #'(cheat-grep `arg '(
                                    dev/cheat/cmds/findgrep.org
                                    ))])))

(define-line-macro crg
  (lambda (stx)
    (syntax-parse stx
      [(_ arg) #'(cheat-grep `arg '(
                                    dev/cheat/cmds/git.org
                                    ))])))

(define-line-macro crn
  (lambda (stx)
    (syntax-parse stx
      [(_ arg) #'(cheat-grep `arg '(
                                    dev/notes/category-theory.org
                                    dev/notes/computer-sciences-theory.org
                                    dev/notes/computer-sciences-engineering.org
                                    dev/notes/logics.org
                                    dev/notes/math.org
                                    dev/notes/notes.org
                                    dev/notes/math-structures.org
                                    ))])))
(define-line-macro cru
  (lambda (stx)
    (syntax-parse stx
      [(_ arg) #'(simple-grep `arg '(
                                     dev/cheat/cmds/utf8.txt
                                     ))])))

(define-line-macro crv
  (lambda (stx)
    (syntax-parse stx
      [(_ arg) #'(cheat-grep `arg '(
                                    dev/cheat/cmds/vim.org
                                    ))])))
