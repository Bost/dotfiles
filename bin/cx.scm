(use-modules (utils)
             )
;; Example:
;;     chmod --recursive u=rwx,g=rwx,o=rwx /path/to/dir

(define (main args)
  (exec (cons* "chmod +x" (cdr args))))
