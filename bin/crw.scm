(use-modules (utils)
             )
;; Example:
;;     chmod --recursive u=rwx,g=rwx,o=rwx /path/to/dir

(define (main args)
  (exec (cons* "chmod +rw" (cdr args))))

#;(apply
 system*
 ((compose
   (lambda (cmd)
     "(cdr (command-line)) can be an empty list which breaks system*. `append'
takes care of it"
     (append cmd (cdr (command-line)))))
  (list (string-append (getenv "systemBinDir") "/chmod")
        "+w")))
