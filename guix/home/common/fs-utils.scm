(define-module (fs-utils)
  #:use-module (utils)
  #:use-module (settings)
  ;; program-file local-file
  #:use-module (guix gexp)
  ;; take remove delete-duplicates append-map etc.
  #:use-module (srfi srfi-1)
  #:export (
            user-home
            user-dev
            user-dotf
            ))

(define m (module-name-for-logging))
(evaluating-module)

(define* (user-home #:rest args) (apply str home args))
(define-public dev (user-home "/dev"))

(define* (user-dev #:rest args)  (apply str dev args))
(define-public dotf (user-dev "/dotfiles"))
(define* (user-dotf #:rest args) (apply str dotf args))

(define-public dtf (user-dev "/dotfiles"))
(define* (user-dtf #:rest args) (apply str dtf args))

(define-public dtfg (user-dev "/dotfiles/guix"))
(define* (user-dtfg #:rest args) (apply str dtfg args))

(define-public dgx  (user-dev "/guix"))
(define-public dgxp (user-dev "/guix-packages"))

;; see gnu/home/services/symlink-manager.scm
(define-public xdg-config-home (or (getenv "XDG_CONFIG_HOME")
                                   (user-home "/.config")))

(define-public channels-scm "channels.scm")
(define-public channels-scm-relpath
  (str (basename xdg-config-home) "/guix/" channels-scm))

(define-public (fix-leading-dot filename)
  (string-replace filename "dot-" 0 1))

(define-public (local-dotfile path filename)
  "See also (@(services home-dir-config) host-specific-config).

(local-dotfile \"/guix/home/\" \".dir-locals.el\") ; with '.' before file-name
;; =>
(list \".dir-locals.el\"
      (local-file \"/home/bost/dev/dotfiles/guix/home/.dir-locals.el\"
                  \"dot-dir-locals.el\"))

(local-dotfile \"/guix/home/\" \"home-ecke.scm\")
;; =>
(list \"home-ecke.scm\"
      (local-file \"/home/bost/dev/dotfiles/guix/home/home-ecke.scm\"
                  \"home-ecke.scm\"))

(local-dotfile \"/\" \"guix/home/.guile\")
;; =>
(list \"guix/home/.guile\"
      (local-file \"/home/bost/dev/dotfiles/guix/home/.guile\"
                  \"dot-guile\")
"
  (let [(filepath (user-dotf path filename))]
    (if (access? filepath R_OK)
      (list filename
            (let [(base-filename (basename filename))]
              ;; 'local-file' is a macro and cannot be used by 'apply'
              (if (equal? "." (substring base-filename 0 1))
                  ;; base-filename of the local-file can't start with '.'
                  ;; use the `(...) forms for debugging
                  ;; `(local-file ,filepath ,(fix-leading-dot base-filename))
                  ;; `(local-file ,filepath)
                  (local-file filepath (fix-leading-dot base-filename))
                  (local-file filepath))))
      (begin
        (format #t "E ~a Can't read ~a\n" m filepath)
        #f))))

;; fish and bash separate elements of a list with a different separator
(define-public list-separator-bash ":")
#;(define list-separator-fish " ") ;; not needed
(define-public bin-dirpath "/bin")
(define-public sbin-dirpath "/sbin")
(define-public scm-bin-dirname "scm-bin")
(define-public scm-bin-dirpath (str "/" scm-bin-dirname))

(module-evaluated)
