
;; run by `guile l.scm'

;; On non-Guix OS
;; #!/usr/bin/guile -s
;; !#

;; On GuixOS:
;; #!/run/current-system/profile/bin/guile -s
;; !#

;; see also https://the.exa.website/ - A modern replacement for ls.

;; # exa initial run is slow and in general it takes a moment on a weaker HW
;; # ~/bin/exa is a link to ~/bin/exa-linux-x86_64
;; set cmd exa --long --git --all --time-style=long-iso $escArgv
;; # set cmd exa --long --git --all --extended --time=modified $escArgv

(apply
 system*
 ((compose
   (lambda (cmd)
     "(cdr (command-line)) can be an empty list which breaks system*. `append'
takes care of it"
     (append cmd (cdr (command-line)))))
  (list (string-append (getenv "systemBinDir") "/ls")
        "-lA"
        "--file-type" ; append indicator (one of /=>@|) to entries
        ;; TODO consider custom coloring after `ls --color=never`
        "--color" ; must be used
        "--time-style=+%d-%m-%Y %H:%M:%S")))
