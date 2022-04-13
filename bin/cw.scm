
;; run by `guile cx.scm'

;; On non-Guix OS
;; #!/usr/bin/guile -s
;; !#

;; On GuixOS:
;; #!/run/current-system/profile/bin/guile -s
;; !#

;; Example:
;;     chmod --recursive u=rwx,g=rwx,o=rwx /path/to/dir

(apply
 system*
 ((compose
   (lambda (cmd)
     "(cdr (command-line)) can be an empty list which breaks system*. `append'
takes care of it"
     (append cmd (cdr (command-line)))))
  (list (string-append (getenv "systemBinDir") "/chmod")
        "+w")))
