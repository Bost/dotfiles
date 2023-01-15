(define-module (srvc my=fish)
  ;; See service-file -> with-imported-modules
  #:use-module (utils)
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
  #:export (
            my=fish-service
            m2=fish-service
            ))

(define indent "")
(define indent-inc "   ")

(def* (dotfiles-home #:rest args)
  "Note:
(format #t \"~a\" \"foo\") doesn't work"
  (apply str home "/dev/dotfiles" args))

(def* (fish-config-base #:rest args)
  "(fish-config-base) ; => \".config/fish\""
  (apply str (basename xdg-config-home) "/fish" args))

(def* (fish-config-dotfiles #:rest args)
  "(fish-config-dotfiles) ; => \"/home/bost/dev/dotfiles/.config/fish\"
Note:
(format #t \"~a\" \"foo\") doesn't work"
  (apply str (dotfiles-home) "/" (fish-config-base) args))

;;; TODO The (copy-file ...) is not an atomic operation, i.e. it's not undone
;;; when the 'guix home reconfigure ...' fails or is interrupted.
;;; Can't use `local-file' or `mixed-text-file' or something similar since the
;;; `fish_variables' must be editable
(let* [(filepath "/fish_variables")
       (src (fish-config-dotfiles filepath))
       (dst (user-home "/" (fish-config-base filepath)))]
;;; TODO is this sexp is not executed because of lazy-evaluation?
  (let [(indent (str indent indent-inc))]
    (format #t "~a(copy-file ~a ~a) ... " indent src dst)
    (let ((retval (copy-file src dst)))
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

(def* (append-fish-config-dir dir lst)
  (append
   `((,(fish-config-base dir)
      ,(local-file (fish-config-dotfiles dir)
                   #:recursive? #t)))
   lst))


(define (my=serialize-fish-aliases field-name val)
  ;; (format #t "[serialize-fish-aliases] field-name: ~a; val: ~a\n" field-name val)
  #~(string-append
     #$@(map (match-lambda
               ((key . value)
                #~(string-append "alias " #$key " \"" #$value "\"\n"))
               (_ ""))
             val)))

(define (my=serialize-fish-abbreviations field-name val)
  ;; (format #t "[serialize-fish-abbreviations] field-name: ~a; val: ~a\n" field-name val)
  #~(string-append
     #$@(map (match-lambda
               ((key . value)
                #~(string-append "abbr --add " #$key " " #$value "\n"))
               (_ ""))
             val)))

(define (my=serialize-fish-env-vars field-name val)
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

(define-configuration my=home-fish-configuration
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
   my=serialize-fish-env-vars)
  (aliases
   (alist '())
   "Association list of aliases for Fish, both the key and the value
should be a string.  An alias is just a simple function that wraps a
command, If you want something more akin to @dfn{aliases} in POSIX
shells, see the @code{abbreviations} field."
   my=serialize-fish-aliases)
  (abbreviations
   (alist '())
   "Association list of abbreviations for Fish.  These are words that,
when typed in the shell, will automatically expand to the full text."
   my=serialize-fish-abbreviations)
  )

(define (my=fish-packages config)
  "Defines how is the `home-profile' (i.e. the `home-profile-service-type') extended."
  (let ((ret (list (my=home-fish-configuration-package config))))
    (format #t "### [my=fish-packages] ret: \n~a\n\n" ret)
    ret))

(define-configuration/no-serialization my=home-fish-extension
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

(define (my=fish-config-files config)
  "Defines how is the `home-xdg-configuration' (i.e. the `home-xdg-configuration-files-service-type') extended"
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
                   config my=home-fish-configuration-fields))))))
    (format #t "### [my=fish-config-files] ret: \n~a\n\n" ret)
    ret))

(define (my=home-fish-extensions original-config extension-configs)
  "`home-fish-extensions' defines how the value of the service is extended with the composition of the extensions"
  (format #t "### [my=home-fish-extensions] original-config: \n~a\n\n" original-config)
  (format #t "### [my=home-fish-extensions] extension-configs: \n~a\n\n" extension-configs)
  (let ((ret (my=home-fish-configuration
              (inherit original-config)
              (config
               (append (my=home-fish-configuration-config original-config)
                       (append-map
                        my=home-fish-extension-config extension-configs)))
              (environment-variables
               (append (my=home-fish-configuration-environment-variables original-config)
                       (append-map
                        my=home-fish-extension-environment-variables extension-configs)))
              (aliases
               (append (my=home-fish-configuration-aliases original-config)
                       (append-map
                        my=home-fish-extension-aliases extension-configs)))
              (abbreviations
               (append (my=home-fish-configuration-abbreviations original-config)
                       (append-map
                        my=home-fish-extension-abbreviations extension-configs))))))
    (format #t "### [my=home-fish-extensions] ret: \n~a\n\n" ret)
    ret))

(define my=home-fish-service-type
  (service-type (name 'my=home-fish)
                (extensions
                 (list
                  (service-extension
                   home-xdg-configuration-files-service-type
;;; this function returns all the configuration files of the fish-shell from the
;;; home-xdg-configuration-files-service, i.e. config, environment-variables,
;;; aliases and abbreviations

;;; TODO what about the content of the completions directory? Should it return
;;; too?
                   my=fish-config-files)
                  (service-extension
                   home-profile-service-type
;;; this function returns all the installed fish-shell packages from the
;;; home-profile-service
                   my=fish-packages)

                  ))

                (compose identity)

                (extend
;;; `my=home-fish-extensions' defines how the value of the service is extended
;;; with the composition of the extensions
                 my=home-fish-extensions)
                (default-value (my=home-fish-configuration))
                (description "my=home-fish-service-type with completions.")))

(define (m2=fish-config-files config)
  (let* ((m2= `(("fish/completions"
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
         (ret (append my=home-fish-configuration-fields m2=))
         )
    (format #t "### [m2=fish-config-files] ret: \n~a\n\n" ret)
    ret))

(define m2=home-fish-service-type
  (service-type (name 'm2=home-fish)
                (extensions
                 (list
                  (service-extension
                   my=home-fish-service-type
                   m2=fish-config-files)
                  ))
                (description "m2=home-fish-service-type with completions.")))

(define my=fish-service
  (service
   my=home-fish-service-type
   #;home-fish-service-type
   ;; fish configuration - see ~/dev/guix/gnu/home/services/shells.scm
   (my=home-fish-configuration
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
