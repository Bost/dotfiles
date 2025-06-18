#|
guix repl -L /home/bost/dev/dotfiles/guix -L /home/bost/dev/dotfiles/guix/common -L /home/bost/dev/dotfiles/guix/home/common /home/bost/dev/dotfiles/guix/render-manifest.scm
guix package --cores=24 --profile=/home/bost/.guix-profile --manifest=/home/bost/dev/dotfiles/profile-manifest-evaluated.scm
|#

;; render-manifest.scm
(use-modules (guix profiles)
             (srfi srfi-1)
             (ice-9 pretty-print))  ; enables pretty-print

(define output-file "/home/bost/dev/dotfiles/profile-manifest-evaluated.scm")

(define my-manifest
  (load "/home/bost/dev/dotfiles/guix/profile-manifest.scm"))

(define specs
  (map (lambda (entry)
         (let ((name (manifest-entry-name entry))
               (version (manifest-entry-version entry))
               (output (manifest-entry-output entry)))
           (if output
               (string-append name ":" output)
               name)))
       (manifest-entries my-manifest)))

(call-with-output-file output-file
  (lambda (port)
    (display "(use-modules (guix profiles))\n\n" port)
    (pretty-print
     `(specifications->manifest
       (list
        ,@specs))
     port)))

(format #t "~a created.\n" output-file)
