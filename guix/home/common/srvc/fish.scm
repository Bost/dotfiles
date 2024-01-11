(define-module (srvc fish)
  ;; See service-file -> with-imported-modules
  #:use-module (settings)
  #:use-module (utils)
  #:use-module (fs-utils)
  #:use-module (gnu services)
  #:use-module (guix gexp)               #| program-file local-file |#
  #:use-module (gnu home services)       #| simple-service |#
  ;; #| take remove delete-duplicates append-map etc. |#
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages shells)     #| #$fish-foreign-env |#
  #:use-module (ice-9 pretty-print)      #| pretty-print |#
  #:use-module (gnu services configuration)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
)

;;; `guix home reconfigure` keeps on adding fish-shell abbreviations instead of
;;; recreating them. Consequently the `guix home roll-back` and `guix home
;;; switch-generation` don't correctly restore the previous state.
;;;
;;; "Each abbreviation is stored in its own global or universal variable"
;;; https://fishshell.com/docs/current/cmds/abbr.html#internals
;;; See also https://issues.guix.gnu.org/30265

(define m (module-name-for-logging))
(evaluating-module m)

(define indent "")
(define indent-inc "   ")

(define* (user-dotf #:rest args)
  "Note:
(format #t \"~a\" \"foo\") doesn't work"
  (apply str home "/dev/dotfiles" args))

(define* (fish-config-base #:rest args)
  "(fish-config-base) ; => \".config/fish\""
  (apply str (basename xdg-config-home) "/fish" args))

(define* (fish-config-dotfiles #:rest args)
  "(fish-config-dotfiles) ; => \"/home/bost/dev/dotfiles/.config/fish\"
Note:
(format #t \"~a\" \"foo\") doesn't work"
  (apply str (user-dotf) "/" (fish-config-base) args))

;;; TODO The (copy-file ...) is not an atomic operation, i.e. it's not undone
;;; when the 'guix home reconfigure ...' fails or is interrupted.
;;; Can't use `local-file' or `mixed-text-file' or something similar since the
;;; `fish_variables' must be editable
(let* [(filepath "/fish_variables")
       (src (fish-config-dotfiles filepath))
       (dst (user-home "/" (fish-config-base filepath)))
       (dstdir (dirname dst))]
  (unless (file-exists? dstdir)
    (let [(indent (str indent indent-inc))]
      (format #t "~a(mkdir ~a) ... " indent dstdir)
      (let ((retval (mkdir dstdir)))
        (format #t "retval: ~a\n" retval)
        ;; The value of 'retval' is '#<unspecified>'
;;; TODO continuation: executing the block only if the dstdir was created.
        retval)))
;;; TODO is this sexp is not executed because of lazy-evaluation?
  (let [(indent (str indent indent-inc))]
    (format #t "~a(copy-file ~a ~a) ... " indent src dst)
    (let ((retval (copy-file src dst))) ;; consider using `symlink'
      (format #t "retval: ~a\n" retval)
      ;; The value of 'retval' is '#<unspecified>'
      retval))
;;; Just changing ownership and permissions of `fish_variables' doesn't work:
  #;
  (begin
    ;; .rw-r--r-- bost users fish_variables
    (format #t "(chown ~a ~a ~a)\n" dst (getuid) (getgid))
    (chown dst (getuid) (getgid))
    ;; .rw-r--r-- fish_variables
    (format #t "(chmod ~a ~a)\n" dst #o644)
    (chmod dst #o644)))

(define* (append-fish-config-dir dir lst)
  (append
   `((,(fish-config-base dir)
      ,(local-file (fish-config-dotfiles dir)
                   #:recursive? #t)))
   lst))

(define (serialize-fish-aliases field-name val)
  ;; (format #t "[serialize-fish-aliases] field-name: ~a; val: ~a\n" field-name val)
  #~(string-append
     #$@(map (match-lambda
               ((key . value)
                #~(string-append "alias " #$key " \"" #$value "\"\n"))
               (_ ""))
             val)))

(define (serialize-fish-abbreviations field-name val)
  ;; (format #t "[serialize-fish-abbreviations] field-name: ~a; val: ~a\n" field-name val)
  #~(string-append
     #$@(map (match-lambda
               ((key . value)
                #~(string-append "abbr --add " #$key " " #$value "\n"))
               (_ ""))
             val)))

(define (serialize-fish-env-vars field-name val)
  ;; (format #t "[serialize-fish-env-vars] field-name: ~a; val: ~a\n" field-name val)
  #~(string-append
     #$@(map (match-lambda
               ((key . #f)
                "")
               ((key . #t)
                #~(string-append "set -x " #$key "\n"))
               ((key . value)
                #~(string-append "set -x " #$key " "  #$value "\n")))
             val)))

(define-configuration home-fish-config
  (package
    (package fish)
    "The Fish package to use.")
  (config
   (text-config '())
   "List of file-like objects, which will be added to
@file{$XDG_CONFIG_HOME/fish/config.fish}.")
  (environment-variables
   (alist '())
   "Association list of environment variables to set in Fish."
   (serializer serialize-fish-env-vars))
  (aliases
   (alist '())
   "Association list of aliases for Fish, both the key and the value
should be a string.  An alias is just a simple function that wraps a
command, If you want something more akin to @dfn{aliases} in POSIX
shells, see the @code{abbreviations} field."
   (serializer serialize-fish-aliases))
  (abbreviations
   (alist '())
   "Association list of abbreviations for Fish.  These are words that,
when typed in the shell, will automatically expand to the full text."
   (serializer serialize-fish-abbreviations))
  )

(define (fish-packages config)
  "Defines how is the `home-profile' (i.e. the `home-profile-service-type')
extended."
  (let ((ret (list (home-fish-config-package config))))
    ;; (format #t "### [fish-packages] ret: \n~a\n\n" ret)
    ret))

(define-configuration/no-serialization home-fish-extension
  (config
   (text-config '())
   "List of file-like objects for extending the Fish initialization file.")
  (environment-variables
   (alist '())
   "Association list of environment variables to set.")
  (aliases
   (alist '())
   "Association list of Fish aliases.")
  (abbreviations
   (alist '())
   "Association list of Fish abbreviations.")
  )

(define (fish-config-files config)
  "Defines how is the `home-xdg-configuration' (i.e. the
`home-xdg-configuration-files-service-type') extended"
     #|
     (partial append
              `((,(fish-config-base)
;;; TODO modify `local-file' so that can copy files from /gnu/store with
                 ;; different stats, not only as '.r--r--r-- root root'
                 ,(local-file (fish-config-dotfiles)
                              #:recursive? #t
                              #:select?
                              (lambda (file stats)
                                (let* [(ret (or
;;; `fish_prompt.fish' (among others) changes the content of `fish_variables' so
;;; this file must be present and editable otherwise all sorts of
;;; 'Permission denied' problems are to be expected.

                                             (has-suffix? file "/fish_variables")
;;; `config.fish' is copied by `home-fish-configuration'
                                             (has-suffix? file "/config.fish")))]
                                  (when ret
                                    (format #t "excluding: ~a ~a\n" file stats))
                                  (not ret)))))))
     |#
  (let ((ret `(("fish/completions"
                ,(local-file (fish-config-dotfiles "/completions")
                             #:recursive? #t))
               ("fish/conf.d"
                ,(local-file (fish-config-dotfiles "/conf.d")
                             #:recursive? #t))
               ("fish/functions"
                ,(local-file (fish-config-dotfiles "/functions")
                             #:recursive? #t))
               ("fish/fish_plugins"
                ;; fish_plugins is just a file, not a directory
                ,(local-file (fish-config-dotfiles "/fish_plugins")))
               ("fish/config.fish"
                ,(mixed-text-file
                  "fish-config.fish"
                  #~(string-append "\
# if we haven't sourced the login config, do it
status --is-login; and not set -q __fish_login_config_sourced
and begin

  set --prepend fish_function_path "
                                   #$fish-foreign-env
                                   "/share/fish/functions
  fenv source $HOME/.profile
  set -e fish_function_path[1]

  set -g __fish_login_config_sourced 1

end\n\n")
                  (serialize-configuration
                   config home-fish-config-fields))))))
    ;; (format #t "### [fish-config-files] ret: \n~a\n\n" ret)
    ret))

(define (home-fish-extensions original-config extension-configs)
  "`home-fish-extensions' defines how the value of the service is extended with
the composition of the extensions"
  ;; (format #t "### [home-fish-extensions] original-config: \n~a\n\n" original-config)
  ;; (format #t "### [home-fish-extensions] extension-configs: \n~a\n\n" extension-configs)
  (let ((ret (home-fish-config
              (inherit original-config)
              (config
               (append (home-fish-config-config original-config)
                       (append-map
                        home-fish-extension-config extension-configs)))
              (environment-variables
               (append (home-fish-config-environment-variables original-config)
                       (append-map
                        home-fish-extension-environment-variables extension-configs)))
              (aliases
               (append (home-fish-config-aliases original-config)
                       (append-map
                        home-fish-extension-aliases extension-configs)))
              (abbreviations
               (append (home-fish-config-abbreviations original-config)
                       (append-map
                        home-fish-extension-abbreviations extension-configs))))))
    ;; (format #t "### [home-fish-extensions] ret: \n~a\n\n" ret)
    ret))

(define home-fish-service-type
  (service-type (name 'home-fish)
                (extensions
                 (list
                  (service-extension
                   home-xdg-configuration-files-service-type
;;; this function returns all the configuration files of the fish-shell from the
;;; home-xdg-configuration-files-service, i.e. config, environment-variables,
;;; aliases and abbreviations

;;; TODO what about the content of the completions directory? Should it return
;;; too?
                   fish-config-files)
                  (service-extension
                   home-profile-service-type
;;; this function returns all the installed fish-shell packages from the
;;; home-profile-service
                   fish-packages)
                  ))

                (compose identity)

                (extend
;;; `home-fish-extensions' defines how the value of the service is extended
;;; with the composition of the extensions
                 home-fish-extensions)
                (default-value (home-fish-config))
                (description "home-fish-service-type with completions.")))

(define (fish-config-files2 config)
  (let* ((m2 `(("fish/completions"
                ,(local-file (fish-config-dotfiles "/completions")
                             #:recursive? #t))
               ("fish/conf.d"
                ,(local-file (fish-config-dotfiles "/conf.d")
                             #:recursive? #t))
               ("fish/functions"
                ,(local-file (fish-config-dotfiles "/functions")
                             #:recursive? #t))
               ("fish/fish_plugins"
                ;; fish_plugins is just a file, not a directory
                ,(local-file (fish-config-dotfiles "/fish_plugins")))))
         (ret (append home-fish-config-fields m2))
         )
    ;; (format #t "### [fish-config-files2] ret: \n~a\n\n" ret)
    ret))

(define home-fish-service-type2
  (service-type (name 'home-fish2)
                (extensions
                 (list
                  (service-extension
                   home-fish-service-type
                   fish-config-files2)
                  ))
                (description "home-fish-service-type2 with completions.")))

(define-public fish-service
  (service
   home-fish-service-type
   #;home-fish-service-type
   ;; fish configuration - see ~/dev/guix/gnu/home/services/shells.scm
   (home-fish-config
    ;; Abbreviations are implemented as shell scripts. The TAB key is annoying.
    ;; 1. Erase all abbreviations in the fish-shell:
    ;;     abbr --erase (abbr --list)
    ;; 2. Erase all `_fish_abbr_*' variables from the
    ;;    '.config/fish/fish_variables'

    ;; aliases for "l" "ll" "ls" may be be overridden - see bashrc aliases
    #;(aliases '(("l" . "ls -a")))

    (config
     (list
      (local-file
       (fish-config-dotfiles "/config.fish")))))))

(module-evaluated m)
