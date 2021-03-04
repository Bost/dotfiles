#lang racket

(require
 linea/line-macro
 (for-syntax syntax/parse))

(define-line-macro gk
  (lambda (stx)
    (syntax-parse stx
      [(_)          #'{gitk --all &bg}])))

(define-line-macro ghog
  (lambda (stx)
    (syntax-parse stx
      [(_ arg ...) #'(map (lambda (remote)
                            {
                             git push --verbose $remote `arg ...
                             })
                          '(origin gitlab))])))

(define-line-macro gcl
  (lambda (stx)
    (syntax-parse stx
      [(_ arg ...) #'(lambda ()
                       (with-rash-config
                         {
                          git clone `arg ...
                          ;; (count `arg) |> (nth `arg) |> basename |> filename
                          ;; echo "(count `arg)" (count `arg)
                          ;; echo "(count `arg)"
                          }))])))

(define-line-macro glo
  (lambda (stx)
    (syntax-parse stx
      [(_ arg ...) #'(map (lambda (remote)
                            {
                             git fetch --tags $remote `arg ...
                             git rebase `arg ...
                             })
                          '(origin))])))
