;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (home-configuration)
  #:use-module (cfg packages)
  #:use-module (cfg fish)
  #:use-module (cfg abbreviations)
  #:use-module (cfg mcron)
  #:use-module (utils)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services mcron) #| home-mcron-service-type |#
  #:use-module (gnu home services)       #| simple-service |#
  #:use-module (ice-9 ftw)               #| scandir |#
  #:use-module (ice-9 regex)             #| string-match |#
  #:use-module (guix build utils)        #| invoke |#
  ;; #:use-module (srfi srfi-1)          #| take |#

  ;; the https://issues.guix.gnu.org/51359 has not been merged yet
  #| home-git-service-type |#
  ;; #:use-module (gnu home services version-control)
  )

(define* (xdg-config-home #:rest args)
  (apply string-append (basename (getenv "XDG_CONFIG_HOME")) args))

(define* (user-home #:rest args)
  (apply string-append (getenv "HOME") args))

(define (dotfiles-guix-home s)
  "Note:
(format #t \"~a\" \"foo\") doesn't work"
  (user-home "/dev/dotfiles/guix/home" s))

(define funs
  (map (lambda (f)
         `(,(xdg-config-home "/fish/functions/" f)
           ,(local-file (user-home "/dev/dotfiles/fish/functions/" f)
                        ;; fix the 'guix home: error: invalid name: `...fish''
                        (string-append "fish-function-" f))))
       fish-functions))

(define confds
  (map (lambda (f)
         `(,(xdg-config-home "/fish/conf.d/" f)
           ,(local-file (user-home "/dev/dotfiles/fish/conf.d/" f)
                        (string-append "fish-confd-" f))))
       (list
        "_tide_init.fish")))

(define completions
  (map (lambda (f)
         `(,(xdg-config-home "/fish/completions/" f)
           ,(local-file (user-home "/dev/dotfiles/fish/completions/" f)
                        (string-append "fish-completion-" f))))
       (list
        "fisher.fish"
        "tide.fish")))

(define plugins
  (map (lambda (f)
         `(,(xdg-config-home "/fish/" f)
           ,(local-file (user-home "/dev/dotfiles/fish/" f)
                        (string-append "fish-plugins-" f))))
       (list
        "fish_plugins"
        ;; "fish_variables" this is changed
        )))

;; fish and bash separate elements of a list with a different separator
(define list-separator-bash ":")
(define list-separator-fish " ")
(define scm-bin-dirname "scm-bin")
(define scm-bin-dirpath (string-append "/" scm-bin-dirname))

(define (environment-vars list-separator)
  `(
    ;; used by ghog glog
    ("remotes" . ,(string-join (list "origin" "gitlab")
                               list-separator-bash))

    ;; `guix edit ...' reads $VISUAL and/or $EDITOR environment variables
    ("EDITOR" . "e") ;; which "e": /home/bost/scm-bin/e

    ("PATH" . ,(string-join
                (list
                 ;; my own scripts take precedence...
                 (string-append "$HOME" scm-bin-dirpath)
                 ;; TODO create the link
                 ;;     ln -s ~/dev/dotfiles/bin ~/bin
                 ;; using guix home
                 "$HOME/bin"
                 ;; ... over default default PATH, putting...
                 "$PATH"
                 ;; ... bin-directory for for script-based installations of:
                 ;;     babashka heroku clojure
                 ;; at the end of the PATH
                 "/usr/local/bin")
                list-separator))))

(define (read-module name)
  (let ((name-scm (string-append name ".scm")))
    (scheme-file name-scm
                 (sexp->gexp
                  (call-with-input-file
                      (user-home
                       "/dev/dotfiles/guix/home/" name-scm)
                    read-all-sexprs))
                 #:splice? #t)))

(define module-utils (read-module "utils"))

(define* (service-file program-file-name program-description
                       #:key
                       scheme-file-name
                       module-name)
  "The priority is 1. module-name, 2. scheme-file-name, 3. program-file-name"
  `(,(string-append scm-bin-dirname "/" program-file-name)
    ,(program-file
      program-description
      ;; TODO clarify is source-module-closure needed only for imports of
      ;; guix modules?
      (let ((symb (or module-name
                      (and scheme-file-name
                           (string->symbol scheme-file-name))
                      (string->symbol program-file-name))))
        (with-imported-modules `(((utils) => ,module-utils)
                                 ((,symb) => ,(read-module
                                               (or scheme-file-name
                                                   program-file-name))))
          #~(begin
              (use-modules (#$symb))
              (main (command-line))))))))

(define (search-notes program-name files)
  `(,(string-append scm-bin-dirname "/" program-name)
    ,(program-file
      (string-append "search-notes-" program-name)
      ;; TODO clarify is source-module-closure needed only for imports of
      ;; guix modules?
      (with-imported-modules `(((utils) => ,module-utils)
                               ((search-notes) => ,(read-module
                                                    "search-notes")))
                             #~(begin
                                 (use-modules (search-notes))
                                 (main #$files (command-line)))))))

(define (chmod-plus program-name modifier)
  "Example:
        chmod --recursive u=rwx,g=rwx,o=rwx /path/to/dir"
  `(,(string-append scm-bin-dirname "/" program-name)
    ,(program-file
      (string-append "chmod-plus-" modifier)
      ;; TODO clarify is source-module-closure needed only for imports of
      ;; guix modules?
      (with-imported-modules `(((utils) => ,module-utils)
                               ((chmod) => ,(read-module "chmod")))
        #~(begin
            (use-modules (chmod))
            (main #$modifier (command-line)))))))

(home-environment
 (packages
  (map (compose list specification->package+output)
       user-profile-packages))

 ;; TODO
 ;; see [PATCH] services: Add udev-rules-service helper. https://issues.guix.gnu.org/40454

 (services
  (list
   (service
    home-bash-service-type
    (home-bash-configuration
     ;; (guix-defaults? #t) ;; Add sane defaults to the top of the .bashrc
     #|
     ;; Aliases will be defined after the contents of the bashrc field has been
     ;; put in the .bashrc
     ;; TODO fix the documentation:
     ;; The aliases are on the top of the .bashrc (b/c of '(guix-defaults? #t)' ???)
     |#
     ;; When using 'bashrc - local-file' then the aliases are added to the
     ;; .bashrc at the bottom.
     ;; When using '(guix-defaults? #t)' then the aliases are on the top of the
     ;; .bashrc.
     (aliases
       ;; aliases for "l" "ll" "ls" come from the .bashrc template and will be
       ;; overridden because see above
      '())

     ;; List of file-like objects, which will be ADDED(!) to .bashrc.
     (bashrc
      (list
       (local-file
        ;; (local-file ".bashrc" "bashrc") should work too
        (dotfiles-guix-home "/.bashrc_additions")
        ;; prevent 'guix home: error: invalid name: `.bashrc''
        "bashrc_additions")))
     ;; List of file-like objects, which will be ADDED(!) to .bash_profile
     (bash-profile
      (list
       (plain-file "bash-profile"
                   (string-append
                    "\n" "export HISTFILE=$XDG_CACHE_HOME/.bash_history"))
       #;
       (local-file
       ;; (local-file ".bashrc" "bash_profile") should work too
       (dotfiles-guix-home "/.bash_profile_additions")
       ;; prevent 'guix home: error: invalid name: `.bash_profile''
       "bash_profile_additions")))
     (environment-variables
      (environment-vars list-separator-bash))))

   ;; emacs-with-native-comp
   ;; https://github.com/flatwhatson/guix-channel/blob/master/flat/packages/emacs.scm

   ;; https://github.com/search?q=home-fish-service-type&type=code
   ;; see https://github.com/babariviere/brycus/blob/e22cd0c0b75c5b4c95369fc95cce95ed299b63ff/guix/brycus/home-service.scm
   (service
    home-fish-service-type
    ;; fish configuration - see gnu/home/services/shells.scm

    ;; see /home/bost/dev/guix/gnu/home/services/shells.scm
    (home-fish-configuration
     (abbreviations abbrevs)
     #;
     (aliases
     '(
     #;("l" . "ls -a")
     ("dev"   . "cd $HOME/dev")
     ("dec"   . "cd $HOME/dec")
     ("der"   . "cd $HOME/der")
     ("bin"   . "cd $HOME/bin")
     ("cheat" . "cd $HOME/dev/cheat")
     ("dotf"  . "cd $HOME/dev/dotfiles")))
     (config (list (local-file
                    (user-home "/dev/dotfiles/fish/config.fish"))))
     ;; see also home-environment-variables-service-type
     ;; https://guix.gnu.org/manual/devel/en/html_node/Essential-Home-Services.html
     ;; (simple-service 'some-useful-env-vars-service
     ;;                 home-environment-variables-service-type
     ;;                 `(("LESSHISTFILE" . "$XDG_CACHE_HOME/.lesshst")
     ;;                   ("SHELL" . ,(file-append zsh "/bin/zsh"))
     ;;                   ("USELESS_VAR" . #f)
     ;;                   ("_JAVA_AWT_WM_NONREPARENTING" . #t)))

     (environment-variables
      (environment-vars list-separator-fish))))

   (simple-service 'local-stuff-config
                   home-files-service-type
                   (cons
                    (let ((fname "local-stuff.fish"))
                      (list fname
                       (local-file (user-home
                                    "/dev/dotfiles/guix/home/" fname))))
                    funs
                    #;
                    (append plugins (append funs (append completions
                                                         confds)))))

   (simple-service
    'scheme-files home-files-service-type
    (list
     (search-notes "crc"  "clojure")
     (search-notes "cre"  "vim|emacs|org_mode")
     (search-notes "crep" ".*")
     (search-notes "crf"  "find_and_grep")
     (search-notes "crg"  "guix|guile")
     (search-notes "crgi" "git")
     (search-notes "crl"  "guix|shells|linux|android")
     (search-notes "crr"  "racket")
     (search-notes "crs"  "shells")
     (search-notes "cru"  "utf8")
     (chmod-plus   "prw"  "rw")
     (chmod-plus   "px"   "x")
     (service-file "c"       "batcat" #:scheme-file-name "batcat")
     (service-file "l"       "list-directory-contents" #:scheme-file-name "ls")
     (service-file "spag"    "spacemacs-git-fetch-rebase")
     (service-file "ghog"    "git-push-to-remotes")
     (service-file "glo"     "git-fech-and-rebase-from-origin")
     (service-file "qemu-vm" "qemu-virt-machine")
     (service-file "e"       "emacs-launcher" #:scheme-file-name "emacs-launcher")
    ))

   #;
   (simple-service
    'bin-files home-files-service-type
    (map (lambda (f)
           `(,(string-append "bin/" f)
             ,(local-file (user-home "/dev/dotfiles/bin/" f)
                          (string-append "bin-" f))))
         (list "g1" "g1.scm"
               "guix-os" "guix-os.scm"
               "ubuntu-os" "ubuntu-os.scm"
              )))

   #;mcron-service

   ;; https://github.com/babariviere/dotfiles/blob/1deae9e15250c86cc235bb7b6e69ea770af7b13a/baba/home/gaia.scm
   ;; (service home-git-service-type
   ;;          (home-git-configuration
   ;;           (config
   ;;            `((user
   ;;               ((name . "Rostislav Svoboda")
   ;;                (email . "Rostislav.Svoboda@gmail.com")
   ;;                #;(signingKey . "...")))
   ;;              (github
   ;;               ((user . "Bost")))
   ;;              (remote
   ;;               ((pushDefault . "origin")))
   ;;              #;(commit ((gpgSign . #t)))
   ;;              #;(tag ((gpgSign . #t)))))))
   )))
