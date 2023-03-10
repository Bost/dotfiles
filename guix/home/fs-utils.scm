;; (format #t "[fs-utils] evaluating module ...\n")
(define-module (fs-utils)
  #:use-module (utils)
  ;; program-file local-file
  #:use-module (guix gexp)
  ;; take remove delete-duplicates append-map etc.
  #:use-module (srfi srfi-1)
  #:export (
            channels-scm-filepath
            fix-leading-dot
            dotfiles-home
            any-local-file
            local-dotfile
            list-separator-bash
            bin-dirpath
            sbin-dirpath
            scm-bin-dirname
            scm-bin-dirpath
            dev
            ))

(define m (module-name-for-logging))
;; (format #t "~a evaluating module ...\n" m)

(define channels-scm-filepath
  (str (basename xdg-config-home) "/guix/channels.scm"))

(define (fix-leading-dot filename)
  (string-replace filename "dot-" 0 1))

(define* (dotfiles-home #:rest args)
  "Note:
(format #t \"~a\" \"foo\") doesn't work"
  (apply str home "/dev/dotfiles" args))

;;;      ...                      #:optional (<parameter-name> <default-value>)
(define* (any-local-file filepath #:optional (filename (basename filepath)))
  ;; 'local-file' is a macro and cannot be used by 'apply'

  (if (equal? "." (substring filename 0 1))
      ;; filename of the local-file can't start with '.'
      ;; use the `(...) forms for debugging
      ;; `(local-file ,filepath ,(fix-leading-dot filename))
      ;; `(local-file ,filepath)
      (local-file filepath (fix-leading-dot filename))
      (local-file filepath)
      ))

(define (local-dotfile path filename)
  "
(local-dotfile \"/path/to/\" \"file.ext\")
=>
(list \"file.ext\"
      (local-file \"/home/bost/dev/dotfiles/path/to/file.ext\"))

(local-dotfile \"/path/to/\" \".file.ext\")
=>
(list \".file.ext\"
      (local-file
       \"/home/bost/dev/dotfiles/path/to/.file.ext\" \"dot-file.ext\"))

(local-dotfile \"/path/to/\" \".file\")
=>
(list \".file\"
      (local-file \"/home/bost/dev/dotfiles/path/to/.file\" \"dot-file\"))

(local-dotfile \"/\" \"path/to/file.ext\")
=>
(\"path/to/file.ext\" (local-file \"/home/bost/dev/dotfiles/path/to/file.ext\"))

"
  (let [(filepath (dotfiles-home path filename))]
    (if (access? filepath R_OK)
      (list filename
            (any-local-file filepath (basename filename)))
      (begin
        (format #t "ERROR: can't read ~a\n" filepath)
        #f))))

;; fish and bash separate elements of a list with a different separator
(define list-separator-bash ":")
#;(define list-separator-fish " ") ;; not needed
(define bin-dirpath "/bin")
(define sbin-dirpath "/sbin")
(define scm-bin-dirname "scm-bin")
(define scm-bin-dirpath (str "/" scm-bin-dirname))
(define dev (user-home "/dev"))

(define (repl)
  (load "/home/bost/dev/dotfiles/guix/home/fs-utils.scm")
  )
;; (format #t "~a module evaluated\n" m)
