;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.


#|

guix shell --development guix help2man git strace --pure
./pre-inst-env guix repl

(getcwd)
(add-to-load-path
 (str home "/dev/guix"))
(add-to-load-path
 (str home "/dev/dotfiles.dev/guix/home"))

;; see 'include', which unlike 'load', also works within nested lexical contexts
,load "/home/bost/dev/dotfiles.dev/guix/home/home-configuration.scm"
(load "/home/bost/dev/dotfiles.dev/guix/home/home-configuration.scm")

(use-modules
 (cfg packages)
 (cfg fish)
 (cfg abbreviations)
 (cfg mcron)
 (utils)
 (gcl)
 #;(gnu home)
 #;(gnu packages)
 #;(gnu services)
 #;(guix gexp)
 #;(gnu home services shells)
 #;(gnu home services mcron) #| home-mcron-service-type |#
 #;(gnu home services)       #| simple-service |#
 #;(ice-9 ftw)               #| scandir |#
 #;(guix build utils))       #| invoke |#

(list "guix/home/cfg/packages.scm"
"guix/home/cfg/fish.scm"
"guix/home/cfg/abbreviations.scm"
#;"guix/home/cfg/mcron.scm"
"guix/home/utils.scm"
"guix/home/gcl.scm"
"guix/home/home-configuration.scm"
)
(map load
     (list "guix/home/cfg/packages.scm"
           "guix/home/cfg/fish.scm"
           "guix/home/cfg/abbreviations.scm"
           #;"guix/home/cfg/mcron.scm"
           "guix/home/utils.scm"
           "guix/home/gcl.scm"
           #;"guix/home/home-configuration.scm"))
|#

(define-module (home-configuration)
  #:use-module (cfg packages)
  #:use-module (cfg fish)
  #:use-module (cfg abbreviations)
  #:use-module (cfg mcron)
  #:use-module (utils)
  #:use-module (gcl)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix gexp)               #| program-file |#
  #:use-module (gnu home services shells)
  #:use-module (gnu home services mcron) #| home-mcron-service-type |#
  #:use-module (gnu home services)       #| simple-service |#
  #:use-module (ice-9 ftw)               #| scandir |#
  ;; #:use-module (ice-9 string-fun)        #| string-replace-substring |#
  #:use-module (guix build utils)        #| invoke |#
  #:use-module (srfi srfi-1)             #| take remove etc. |#
  #:use-module (gnu packages shellutils)

  ;; the https://issues.guix.gnu.org/51359 has not been merged yet
  #| home-git-service-type |#
  ;; #:use-module (gnu home services version-control)
  )

(define* (any-local-file file #:optional (name (basename file)))
  ;; 'local-file' is a macro and cannot be used by 'apply'
  (if (equal? "." (substring name 0 1))
      ;; name of the local-file can't start with '.'
      (local-file file (string-replace name "dot-" 0 1))
      (local-file file)))

(define (local-dotfile path fname)
  (let ((fpath (dotfiles-home path fname)))
    (when (access? fpath R_OK)
      (list
       fname
       (any-local-file fpath fname)))))

(define* (xdg-config-home #:rest args)
  (apply str (basename
              ;; see gnu/home/services/symlink-manager.scm
              (or (getenv "XDG_CONFIG_HOME")
                  (str home "/.config"))) args))

(define* (user-home #:rest args)
  (apply str home args))

(define* (dotfiles-home #:rest args)
  "Note:
(format #t \"~a\" \"foo\") doesn't work"
  (apply str home "/dev/dotfiles" args))

;; TODO look at (local-file ... #:recursive? #t)
(define funs
  (map (lambda (f)
         `(,(xdg-config-home "/fish/functions/" f)
           ,(local-file (dotfiles-home "/fish/functions/" f)
                        ;; fix the 'guix home: error: invalid name: `...fish''
                        (str "fish-function-" f))))
       fish-functions))

(define confds
  (map (lambda (f)
         `(,(xdg-config-home "/fish/conf.d/" f)
           ,(local-file (dotfiles-home "/fish/conf.d/" f)
                        (str "fish-confd-" f))))
       (list
        "_tide_init.fish")))

(define completions
  (map (lambda (f)
         `(,(xdg-config-home "/fish/completions/" f)
           ,(local-file (dotfiles-home "/fish/completions/" f)
                        (str "fish-completion-" f))))
       (list
        "fisher.fish"
        "tide.fish")))

(define plugins
  (map (lambda (f)
         `(,(xdg-config-home "/fish/" f)
           ,(local-file (dotfiles-home "/fish/" f)
                        (str "fish-plugins-" f))))
       (list
        "fish_plugins"
        ;; "fish_variables" this is changed
        )))

;; fish and bash separate elements of a list with a different separator
(define list-separator-bash ":")
(define list-separator-fish " ")
(define scm-bin-dirname "scm-bin")
(define scm-bin-dirpath (str "/" scm-bin-dirname))

;; (define (if-def-prepend var-path-name path)
;;   (define (path-exists? path) #f)
;;   (when (path-exists? path)
;;     `(var-path-name . ,(string-join path var-path-name))))

;; (if-def-prepend "GUILE_LOAD_PATH"
;;                 "$HOME/.guix-profile/share/guile/site/3.0")
;; (if-def-prepend "GUILE_LOAD_COMPILED_PATH"
;;                 "$HOME/.guix-profile/lib/guile/3.0/site-ccache")

;; (define (if-def-prepend var-path-name path)
;;   (define (path-exists? path) #f)
;;   (when (path-exists? path)
;;     `(var-path-name . ,(string-join path var-path-name))))

;; (if-def-prepend "GUILE_LOAD_PATH"
;;                 "$HOME/.guix-profile/share/guile/site/3.0")
;; (if-def-prepend "GUILE_LOAD_COMPILED_PATH"
;;                 "$HOME/.guix-profile/lib/guile/3.0/site-ccache")

(define (environment-vars list-separator)
  `(
    #;("CMAKE_C_COMPILER" . "$HOME/.guix-profile/bin/gcc")
    ("CC" . "$HOME/.guix-profile/bin/gcc")

    ;; rga: ripgrep, plus search in pdf, E-Books, Office docs, zip, tar.gz, etc.
    ;; See https://github.com/phiresky/ripgrep-all
    #;("PATH" . ,(string-join "$HOME/bin/ripgrep_all" "$PATH"))

    ;; Remedy against:
    ;; $ lein uberjar
    ;; Release versions may not depend upon snapshots.
    ;; Freeze snapshots to dated versions or set the LEIN_SNAPSHOTS_IN_RELEASE
    ;; environment variable to override.
    ("LEIN_SNAPSHOTS_IN_RELEASE" . "allowed")

    ;; JAVA_HOME definitions - see (changes require logout & login):
    ;;     /etc/profile.d/jdk.csh
    ;;     /etc/profile.d/jdk.sh
    ;;     /etc/environment
    #;
    ("JAVA_HOME" . (string-append "/usr/lib/jvm/"
    #;"java-8-openjdk-amd64"
    "java-11-openjdk-amd64"))

    ;; Setting the locale correctly:
    ;; https://systemcrafters.cc/craft-your-system-with-guix/installing-the-package-manager/#setting-the-locale-correctly
    ;; When 'setlocale: LC_ALL: cannot change locale'
    ;; ("GUIX_LOCPATH" . "$HOME/.guix-profile/lib/locale")

    ;; TODO move CORONA_ENV_TYPE and REPL_USER to .envrc
    ;; see also $dec/corona_cases/.env and $dec/corona_cases/.heroku-local.env
    ("CORONA_ENV_TYPE" . "devel")
    ("REPL_USER" . "$USER")

    ;; needed by `help`; e.g. `help expand`
    ("BROWSER" . "firefox")

    ;; for `flatpak run ...`
    ("XDG_DATA_DIRS" . ,(string-join
                         (list
                          "$HOME/.local/share/flatpak/exports/share"
                          "/var/lib/flatpak/exports/share"
                          "$XDG_DATA_DIRS")))

    ("dev"   . "$HOME/dev")
    ("dec"   . "$HOME/dec")
    ("der"   . "$HOME/der")
    ("bin"   . "$HOME/bin")
    ("cheat" . "$dev/cheat")
    ("dotf"  . "$dev/dotfiles")

    ;; used by ghog glog
    ("remotes" . ,(string-join (list "origin" "gitlab")
                               list-separator-bash))

    ;; `guix edit ...' reads $VISUAL and/or $EDITOR environment variables
    ("EDITOR" . "e") ;; which "e": /home/bost/scm-bin/e
    ;; TODO test if the library exists:
    ;;   test -e $LDP && set --export LD_PRELOAD $LDP
    ;; ("LD_PRELOAD" . "/usr/lib/x86_64-linux-gnu/libgtk3-nocsd.so.0")

    ("PATH" . ,(string-join
                (list
                 ;; my own scripts take precedence...
                 (str "$HOME" scm-bin-dirpath)
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

(format #t "~a\n" "environment-vars defined")

(define (read-module name)
  "TODO use monad"
  (let ((name-scm (str name ".scm")))
    (format #t "read-module: ~a ... " (dotfiles-home "/guix/home/" name-scm))
    (let ((sf (scheme-file name-scm
                           (sexp->gexp
                            (call-with-input-file
                                (dotfiles-home "/guix/home/" name-scm)
                              read-all-sexprs))
                           #:splice? #t)))
      (format #t "done\n")
      sf)))

(format #t "~a\n" "read-module defined")

(define module-utils (read-module "utils"))

(format #t "~a\n" "module-utils defined")

(define* (service-file #:key
                       program-name desc
                       scheme-file-name module-name)
  "The priority is 1. module-name, 2. scheme-file-name, 3. program-name"
  `(,(str scm-bin-dirname "/" program-name)
    ,(program-file
      desc
      ;; TODO clarify if source-module-closure is needed only for imports of
      ;; guix modules?
      (let* ((symb-string (or scheme-file-name program-name))
             (symb (or module-name
                       (string->symbol symb-string))))
        (with-imported-modules `(((utils) => ,module-utils)
                                 ((,symb) => ,(read-module symb-string)))
                               #~(begin
                                   (use-modules (#$symb))
                                   (main (command-line))))))))

(format #t "~a\n" "service-file defined")

(define* (search-notes #:key program-name files)
  `(,(str scm-bin-dirname "/" program-name)
    ,(program-file
      (str "search-notes-" program-name)
      ;; TODO clarify is source-module-closure needed only for imports of
      ;; guix modules?
      (let* ((symb-string "search-notes")
             (main-1st-arg files))
        (let ((symb (string->symbol symb-string)))
          (with-imported-modules
              `(((utils) => ,module-utils)
                ((,symb) => ,(read-module symb-string)))
            #~(begin
                (use-modules (#$symb))
                (main #$main-1st-arg (command-line)))))))))

(format #t "~a\n" "search-notes defined")

(define* (chmod-plus #:key program-name chmod-params)
  "Example:
        chmod --recursive u=rwx,g=rwx,o=rwx /path/to/dir"
  `(,(str scm-bin-dirname "/" program-name)
    ,(program-file
      (str "chmod-plus-" chmod-params)
      ;; TODO clarify is source-module-closure needed only for imports of
      ;; guix modules?
      (let ((symb-string "chmod")
            (main-1st-arg chmod-params))
        (let ((symb (string->symbol symb-string)))
          (with-imported-modules
              `(((utils) => ,module-utils)
                ((,symb) => ,(read-module symb-string)))
            #~(begin
                (use-modules (#$symb))
                (main #$main-1st-arg (command-line)))))))))

(format #t "~a\n" "chmod-plus defined")

;; xfce4-keyboard: repeat-delay 160 repeat-speed 60

;; "copying files"
;; there should be a service type to place particular files (or file-like
;; objects) to a target destination

;; extend home-activation-service-type or home-run-on-first-login-service-type
;; to run some scripts, required to be idempotent though.

;; use home-files-service-type for copying configurations
;; home-files-service-type example:
#;
(services
 ...
 (list
  (simple-service 'dotfiles-installation
                  home-files-service-type
                  `((".config/zsh" ;; destination
                     ,(local-file
                       "/home/foobar/etc/zsh/.config/zsh" ;; source file/directory
                       "zsh-config"
                       ;; #t to copy directory
                       #:recursive? #t))))))

(define projects
  (list
   (cons "/dec" (list "/corona_cases" "/fdk"))
   (cons "/der" (list "/search-notes"))
   (cons "/dev" (list
                 #;(cons "/guix" "https://git.savannah.gnu.org/git/guix.git")
                 "/notes" "/dotfiles"))))

;; wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
;; ln -s ~/dev/dotfiles/.lein
(define (obtain-and-setup dest-dir repo)
  (let* ((gitlab "git@gitlab.com:rostislav.svoboda")
         (github "git@github.com:Bost")
         (dest-dir-repo (str home dest-dir repo))
         (repo-url
          (if #f ; (url? repo)
              repo
              (str gitlab repo))))
    (gcl "--origin=gitlab" repo-url dest-dir-repo)
    (exec-system*
     "git" (str "--git-dir=" dest-dir-repo "/.git") "remote add github"
     (str github repo))))
(format #t "~a\n" "obtain-and-setup defined")

;; Existing projects won't be overridden
#;
(map (lambda (project)
       (let ((dest-dir (car project)))
         (map (partial obtain-and-setup dest-dir) (cdr project))))
     projects)

(define projects-heroku
  (list
   (cons "/der" (list
                 ;; pictures
                 "/vesmir"
                 ;; tetris
                 "/vojto"))))

(define (obtain-and-setup-heroku dest-dir repo)
  (let* ((heroku "https://git.heroku.com/")
         (dest-dir-repo (str home dest-dir repo))
         (repo-url
          (if #f ; (url? repo)
              repo
              (str heroku repo ".git"))))
    (gcl "--origin=vojto" repo-url dest-dir-repo)))
(format #t "~a\n" "obtain-and-setup-heroku defined")

;; Existing projects won't be overridden
#;
(map (lambda (project)
       (let ((dest-dir (car project)))
         (map (partial obtain-and-setup-heroku dest-dir) (cdr project))))
     projects-heroku)


(map (lambda (project)
       (let ((dest-dir (car project)))
         (map (partial obtain-and-setup dest-dir) (cdr project))))
     projects)

(home-environment
 (packages
  (map (compose list specification->package+output)
       user-profile-packages))

 ;; TODO
 ;; see [PATCH] services: Add udev-rules-service helper.
 ;; https://issues.guix.gnu.org/40454

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

     ;; The aliases are on the top of the .bashrc
     ;; (because of '(guix-defaults? #t)'???)

     |#
     ;; When using 'bashrc - local-file' then the aliases are added to the
     ;; .bashrc at the bottom.
     ;; When using '(guix-defaults? #t)' then the aliases are on the top of the
     ;; .bashrc.


     ;; aliases for "l" "ll" "ls" come from the .bashrc template and will be
     ;; overridden because see above
     #;(aliases '())

     ;; List of file-like objects, which will be ADDED(!) to .bashrc.
     (bashrc
      (list
       (plain-file "bashrc"
                   (str
                    "\n" "#### home-bash-configuration -> bashrc: begin"
                    "\n"
                    ;;; Also https://github.com/oh-my-fish/plugin-foreign-env
                    ;; 1. ~/.guix-home/setup-environment does:
                    ;;     source ~/.guix-home/profile/etc/profile"
                    ;;
                    ;; 2. `guix install` may require:
                    ;;      GUIX_PROFILE=$HOME/.guix-profile
                    ;;       . "$GUIX_PROFILE/etc/profile"
                    ;;    i.e. `. ~/.guix-profile/etc/profile`

                    "\n" "eval \"$(direnv hook bash)\""
                    "\n"
                    "\n" "#### home-bash-configuration -> bashrc: end"
                    ))
       (local-file
        ;; (local-file ".bashrc" "bashrc") should work too
        (dotfiles-home "/guix/home/.bashrc_additions")
        ;; prevent 'guix home: error: invalid name: `.bashrc''
        "bashrc_additions")))
     ;; List of file-like objects, which will be ADDED(!) to .bash_profile
     (bash-profile
      (list
       (plain-file "bash-profile"
                   (str
                    "\n" "export HISTFILE=$XDG_CACHE_HOME/.bash_history"))
       #;
       (local-file
       ;; (local-file ".bashrc" "bash_profile") should work too
       (dotfiles-home "/.bash_profile_additions")
       ;; prevent 'guix home: error: invalid name: `.bash_profile''
       "bash_profile_additions")))
     (environment-variables
      (environment-vars list-separator-bash))))

   ;;; fails with:
   ;;;   In procedure open-file: No such file or directory:
   ;;;   "eval \"$(direnv hook bash)\""
   ;; (simple-service
   ;;  'direnv-bash-hook
   ;;  home-bash-service-type
   ;;  (home-bash-extension
   ;;   (bashrc (list "eval \"$(direnv hook bash)\""))))

   ;; emacs-with-native-comp - ? native compilation ?
   ;; https://github.com/flatwhatson/guix-channel/blob/master/flat/packages/emacs.scm

   ;; https://github.com/search?q=home-fish-service-type&type=code
   ;; see https://github.com/babariviere/brycus/blob/e22cd0c0b75c5b4c95369fc95cce95ed299b63ff/guix/brycus/home-service.scm
   (service
    home-fish-service-type
    ;; fish configuration - see gnu/home/services/shells.scm

    ;; see /home/bost/dev/guix/gnu/home/services/shells.scm
    (home-fish-configuration
     (abbreviations abbrevs)

     ;; aliases for "l" "ll" "ls" may be be overridden - see bashrc aliases
     #;(aliases '(("l" . "ls -a")))

     (config (list (local-file
                    (dotfiles-home "/fish/config.fish"))))
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

   (simple-service 'home-dir-config
                   home-files-service-type
                   (append
                    (remove unspecified?
                            (list
                             ;; TODO notes
                             (local-dotfile "/" ".gitconfig")
                             (local-dotfile "/emacs/" ".spacemacs")
                             (local-dotfile "/guix/home/" "local-stuff.fish")))
                    funs
                    #;
                    (append plugins (append funs (append completions
                                                         confds)))))

   (begin
     (format #t "Running (simple-service 'scheme-files ...) ...\n")
     (let ((ss
            (simple-service
             'scheme-files home-files-service-type
             (list
              (search-notes #:program-name "crc"  #:files "clojure")
              (search-notes #:program-name "cre"  #:files "vim|emacs|org_mode")
              (search-notes #:program-name "crep" #:files ".*")
              (search-notes #:program-name "crf"  #:files "find_and_grep")
              (search-notes #:program-name "crg"  #:files "guix|guile")
              (search-notes #:program-name "crgi" #:files "git")
              (search-notes #:program-name "crl"  #:files "guix|shells|linux|android")
              (search-notes #:program-name "crr"  #:files "racket")
              (search-notes #:program-name "crs"  #:files "shells")
              (search-notes #:program-name "cru"  #:files "utf8")
              (chmod-plus   #:program-name "prw"  #:chmod-params "rw")
              (chmod-plus   #:program-name "px"   #:chmod-params "x")
              (service-file #:program-name "c"       #:desc "batcat"
                            #:scheme-file-name "bat")
              (service-file #:program-name "e"       #:desc "emacs-launcher"
                            #:scheme-file-name "emacs-launcher")
              (service-file #:program-name "f"       #:desc "find-alternative")
              (service-file #:program-name "gcl"     #:desc "git-clone")
              (service-file #:program-name "gco"     #:desc "git-checkout")
              (service-file #:program-name "gcod"
                            #:desc "git-checkout-previous-branch")
              (service-file #:program-name "gcom"    #:desc "git-checkout-master")
              (service-file #:program-name "gg"      #:desc "git-gui")
              (service-file #:program-name "ghog"    #:desc "git-push-to-remotes")
              (service-file #:program-name "gk"      #:desc "git-repository-browser")
              (service-file #:program-name "glo"
                            #:desc "git-fech-and-rebase-from-origin")
              (service-file #:program-name "gs"      #:desc "git-status")
              (service-file #:program-name "gtg"     #:desc "git-tag")
              (service-file #:program-name "l"       #:desc "list-directory-contents"
                            #:scheme-file-name "ls")
              (service-file #:program-name "qemu-vm" #:desc "qemu-virt-machine")
              (service-file #:program-name "spag"    #:desc "spacemacs-git-fetch-rebase")))))
       (format #t "Running (simple-service 'scheme-files ...) ... done\n")
       ss))

   #;mcron-service

   ;; https://github.com/babariviere/dotfiles/blob/1deae9e15250c86cc235bb7b6e69ea770af7b13a/baba/home/gaia.scm
   ;; https://github.com/babariviere/dotfiles/blob/guix/baba/home/gaia.scm
   ;;; [WIP] home: Add home-git-service-type https://issues.guix.gnu.org/54293
   ;;; is not pulled yed
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
