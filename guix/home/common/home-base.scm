(define-module (home-base)
  #:use-module (settings)
  #:use-module (tests)
  #:use-module (utils)
  #:use-module (store-utils)
  #:use-module (memo)

  ;; the code of this module comes in via the 'bost' channel
  ;; #:use-module (bost utils)
  #:use-module (fs-utils)

  #:use-module (cfg packages all) ;; home-packages-to-install
  #:use-module (srvc fish)
  #:use-module (srvc dirs)
  #:use-module (srvc scheme-files)
  #:use-module ((srvc home-dir-cfg) #:prefix srvc:)

  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  ;; program-file local-file
  #:use-module (guix gexp)
  #:use-module (gnu home services shells)
  ;; simple-service
  #:use-module (gnu home services)
  ;; pretty-print
  ;; #:use-module (ice-9 pretty-print)

  ;; the https://issues.guix.gnu.org/51359 has not been merged yet
  ;; home-git-service-type
  ;; #:use-module (gnu home services version-control)

  #:use-module (gnu packages commencement)
)

;; TODO consider putting home and system configurations in one file
;; (if (getenv "RUNNING_GUIX_HOME") home system)

(define m (module-name-for-logging))
(evaluating-module)

(define (bash-config-file name content)
  (plain-file
   name
   (str
    "\n" "### echo \"home-bash-configuration -> " name ": beg $EMACSLOADPATH\""
    "\n"
    content
    "\n"
    "\n" "### echo \"home-bash-configuration -> " name ": end $EMACSLOADPATH\""
    )))
(testsymb 'bash-config-file)

(define (bashrc-str)
  (str
;;; Also https://github.com/oh-my-fish/plugin-foreign-env
;;; 1. ~/.guix-home/setup-environment does:
;;;     source ~/.guix-home/profile/etc/profile"
;;; $ guix package --search-paths --profile=~/.guix-home/profile -I | wc -l
;;; 147
;;;
;;; $ guix package --search-paths --profile=~/.guix-profile -I | wc -l
;;; 160
;;; 2. `guix install` may require:
;;;      GUIX_PROFILE=$HOME/.guix-profile
;;;      source "$GUIX_PROFILE/etc/profile"
;;;    i.e. `. ~/.guix-profile/etc/profile`

   ;; see `info "(gnupg) Invoking GPG-AGENT"`
   "\n" "export GPG_TTY=$(tty)"

   ;; Export empty DIRENV_LOG_FORMAT so that e.g. while desktop
   ;; sharing, it's not obvious what variables are encrypted.
   ;; (Redirect to /dev/null doesn't work.)
   ;; See https://github.com/direnv/direnv/issues/68
   "\n" "export DIRENV_LOG_FORMAT="
   "\n" "eval \"$(direnv hook bash)\""
   "\n"
;;; In bash a script is executes in a subshell, so the cd command only changes
;;; the directory within that subshell. So `gicl` and `mkcd` are implemented as
;;; functions in .bashrc.
   "\n" "# $* vs. $@"
   "\n" "#   \"$*\" concatenates all args into a single string,"
   "\n" "#         separated by the first character of $IFS (normally a space)"
   "\n" "#   \"$@\" treats each arg as its own word."
   "\n" ""
   "\n" "# -- is a POSIX‑style \"end of options\" marker"
   "\n" "gicl() {"
   "\n" "  git-clone -- \"$@\" || return"
   "\n" ""
   "\n" "  # last arg is the URL; strip any trailing slash"
   "\n" "  local url=${!#}"
   "\n" "  url=${url%/}"
   "\n" ""
   "\n" "  # repo name = basename without .git"
   "\n" "  local repo=${url##*/}"
   "\n" "  repo=${repo%.git}"
   "\n" ""
   "\n" "  cd -- \"$repo\" || return"
   "\n" "}"
   "\n"
   "\n" "mkcd() {"
   "\n" "    mkdir -p -- \"$*\" || return"
   "\n" "    cd -- \"$*\" || return"
   "\n" "}"
   "\n"
   "\n" "take() {"
   "\n" "    mkcd \"$@\""
   "\n" "}"
   "\n"
   ;; cd $dotf/.config/fish/functions/
   ;; rg -N -g '*.{fish}' '^[[:blank:]]+cd ' | xsel -bi
   ;; %s#\(.*\).fish:[[:blank:]]+\(.*\)#\1() {\n    \2\n}#g
   ;; %s#\(.*\)#"\\n" "\1"#
   "\n" "..() {"
   "\n" "    cd .."
   "\n" "}"
   "\n" "...() {"
   "\n" "    cd ../.."
   "\n" "}"
   "\n" "....() {"
   "\n" "    cd ../../.."
   "\n" "}"
   "\n" "bin() {"
   "\n" "    cd ~/bin"
   "\n" "}"
   "\n" "cd-() {"
   "\n" "    cd -"
   "\n" "}"
   "\n" "cheat() {"
   "\n" "    cd $cheat"
   "\n" "}"
   "\n" "corona() {"
   "\n" "    cd $dec/corona_cases"
   "\n" "}"
   "\n" "dec() {"
   "\n" "    cd $dec"
   "\n" "}"
   "\n" "dema() {"
   "\n" "    cd $dev/emacs"
   "\n" "}"
   "\n" "der() {"
   "\n" "    cd $der"
   "\n" "}"
   "\n" "desk() {"
   "\n" "    cd ~/Desktop"
   "\n" "}"
   "\n" "dev() {"
   "\n" "    cd $dev"
   "\n" "}"
   "\n" "dgl() {"
   "\n" "    cd $dgl"
   "\n" "}"
   "\n" "dgx() {"
   "\n" "    cd $dgx"
   "\n" "}"
   "\n" "dgxp() {"
   "\n" "    cd $dgxp"
   "\n" "}"
   "\n" "dotf() {"
   "\n" "    cd $dotf"
   "\n" "}"
   "\n" "down() {"
   "\n" "    cd ~/Downloads"
   "\n" "}"
   "\n" "dspc() {"
   "\n" "    cd ~/.emacs.d.distros/spacemacs/cycle/src"
   "\n" "}"
   "\n" "dspd() {"
   "\n" "    cd ~/.emacs.d.distros/spacemacs/develop/src"
   "\n" "}"
   "\n" "dspg() {"
   "\n" "    cd ~/.emacs.d.distros/spacemacs/guix/src"
   "\n" "}"
   "\n" "dtf() {"
   "\n" "    cd $dtf"
   "\n" "}"
   "\n" "dtfg() {"
   "\n" "    cd $dtfg"
   "\n" "}"
   "\n" "latest() {"
   "\n" "    cd ~/.cache/guix/checkouts/$latestRepo"
   "\n" "}"
   "\n" "music() {"
   "\n" "    cd ~/music"
   "\n" "}"
   "\n" "notes() {"
   "\n" "    cd $dev/notes/notes"
   "\n" "}"
   "\n" "owid() {"
   "\n" "    cd $dec/owid"
   "\n" "}"
   "\n" "rr() {"
   "\n" "    cd ~/.config/rash"
   "\n" "}"
   "\n" "tmp() {"
   "\n" "    cd /tmp"
   "\n" "}"
   "\n" "trackle() {"
   "\n" "    cd $dev/trackle"
   "\n" "}"
   "\n" "ufo() {"
   "\n" "    cd $dec/ufo"
   "\n" "}"
   "\n" "utils() {"
   "\n" "    cd $dec/utils"
   "\n" "}"
   "\n" "vesmir() {"
   "\n" "    cd $der/vesmir"
   "\n" "}"
   "\n" "yas() {"
   "\n" "    cd $dev/yasnippet"
   "\n" "}"
   "\n" "zark() {"
   "\n" "    cd $dec/zark"
   "\n" "}"
   ))

;; TODO check if GPG keys are present and show commands how to transfer them:
;; See `crep 'copy\ \/\ transfer'`
(define-public (non-env-var-services)
  (let* [(m (format #f "~a [non-env-var-services]" m))]
    ;; (format #t "~a Starting…\n" m)
    ((comp
      ;; (lambda (v) (format #t "~a done.\n" m) v)
      ;; (lambda (v) (format #t "~a test-type: ~a; length: ~a\n"
      ;;                     m (test-type v) (length v))
      ;;         v)
      (partial
       append
       ;; (service home-xsettingsd-service-type)
       (list
        (service
         home-bash-service-type
         (home-bash-configuration
;;; (guix-defaults? #t) ;; Add sane defaults to the top of the .bashrc

;;; Aliases will be defined after the contents of the bashrc field has been
;;; put in the .bashrc
;;; TODO fix the documentation:

;;; The aliases are on the top of the .bashrc
;;; (because of "(guix-defaults? #t)"???)


;;; When using 'bashrc - local-file' then the aliases are added to the .bashrc
;;; at the bottom. When using '(guix-defaults? #t)' then the aliases are on the
;;; top of the .bashrc.


;;; aliases for "l" "ll" "ls" come from the .bashrc template and will be
;;; overridden because see above
          ;; (aliases '())

          ;; List of file-like objects, which will be ADDED(!) to .bashrc.
          (bashrc
           (list
            (bash-config-file "bashrc" (bashrc-str))
            (let* [(filename ".bashrc_additions")]
              ;; this should work too:
              ;; (local-file ".bashrc" (fix-leading-dot ".bashrc"))
              (local-file
               (user-dotf "/guix/home/" filename)
               (fix-leading-dot filename)))))

          ;; List of file-like objects, which will be ADDED(!) to .bash_profile
          (bash-profile
           (list
            (bash-config-file
             "bash-profile"
             (str
              "\n" "export HISTFILE=$XDG_CACHE_HOME/.bash_history"
              ;; %H:%M:%S can be abbreviated by %T
              "\n" "export HISTTIMEFORMAT=\"[%Y-%m-%d %H:%M:%S] \""
              ;; "\n" "GUIX_PROFILE=$HOME/.guix-profile"
              ;; "\n" "source \"$GUIX_PROFILE/etc/profile\""
              "\n"
              ;; enable all profiles on login
              "\n" "export GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles"
              "\n" "for i in $GUIX_EXTRA_PROFILES/*; do"
              "\n" "  profile=$i/$(basename \"$i\")"
              "\n" "  if [ -f \"$profile\"/etc/profile ]; then"
              "\n" "    GUIX_PROFILE=\"$profile\""
              "\n" "    set -x"
              "\n" "    source \"$GUIX_PROFILE\"/etc/profile"
              "\n" "    { retval=\"$?\"; set +x; } 2>/dev/null"
              "\n" "  fi"
              "\n" "  unset profile"
              "\n" "done"
              ))
            ;; (local-file ".bashrc" "bash_profile") should work too
            ;; (local-file
            ;;  (user-dotf "/.bash_profile_additions")
            ;;  ;; prevent "guix home: error: invalid name: `.bash_profile'"
            ;;  "bash_profile_additions")
            ))))))

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

      ;; (lambda (v) (format #t "~a 6\n" m) v)
      (partial append (list (service dirs-service-type)))
      ;; (lambda (v) (format #t "~a 5\n" m) v)
      (partial append (list (fish-service)))
      ;; (lambda (v) (format #t "~a 3 type: ~a; length: ~a\n" m (test-type v) (length v)) v)
      (partial append (list (srvc:home-dir-cfg-srvc)))
      ;; (lambda (v) (format #t "~a 2 type: ~a; length: ~a\n" m (test-type v) (length v)) v)
      (partial append (list (scheme-files-service)))
      ;; (lambda (v) (format #t "~a 1 type: ~a; length: ~a\n" m (test-type v) (length v)) v)
      ;; (partial append mcron-service)

;;; https://github.com/babariviere/dotfiles/blob/1deae9e15250c86cc235bb7b6e69ea770af7b13a/baba/home/gaia.scm
;;; https://github.com/babariviere/dotfiles/blob/guix/baba/home/gaia.scm
;;; [WIP] home: Add home-git-service-type https://issues.guix.gnu.org/54293 is
;;; not pulled yed

      ;; (partial
      ;;  append
      ;;  (service home-git-service-type
      ;;           (home-git-confqiguration
      ;;            (config
      ;;             `((user
      ;;                ((name . ,user-full-name)
      ;;                 (email . ,user-mail-address)
      ;;                 #;(signingKey . "...")))
      ;;               (github
      ;;                ((user . "Bost")))
      ;;               (remote
      ;;                ((pushDefault . "origin")))
      ;;               #;(commit ((gpgSign . #t)))
      ;;               #;(tag ((gpgSign . #t))))))))
      ;; (lambda (v) (format #t "~a 0\n" m) v)
      )
     (list))))
(testsymb 'non-env-var-services)

(define (gcc-filepath)
  ;; (user-home "/.guix-home/profile/bin/gcc")
  (format #f "~a/bin/gcc" (package-derivation-output!
                           (@(gnu packages commencement) gcc-toolchain))))
(testsymb 'gcc-filepath)

;; See also $dotf/.bashrc.martin
(define-public (environment-vars list-separator)
  ((comp
    #;(lambda (v) (format #t "~a 0\n" m) v))
   `(
     ;; Warn about deprecated Guile features
     ("GUILE_WARN_DEPRECATED" . "detailed")

     ;; CC and CMAKE_C_COMPILER may NOT need be defined in the environment, if
     ;; the "native-compiler-error (libgccjit.so: error invoking gcc driver)"
     ;; doesn't come up.
     ("CC"               . ,(gcc-filepath))
     ("CMAKE_C_COMPILER" . ,(gcc-filepath))

     ("dev"   . ,dev)
     ;; TODO unify value of `bin' with the value in the `PATH' definition
     ("bin"   . ,(user-home bin-dirpath))
     ("cheat" . ,(user-dev "/cheat"))
     ;; for --cores=$cores; see `jobs=$[$(nproc) * 95 / 100]'
     ("cores" . ,(number->string ((@(ice-9 threads) current-processor-count))))
     ("dotf"  . ,(user-dev "/dotfiles"))
     ("dtf"   . ,(user-dev "/dotfiles"))
     ("dtfg"  . ,(user-dev "/dotfiles/guix"))
     ("dgx"   . ,(user-dev "/guix"))
     ("dgxp"  . ,(user-dev "/guix-packages"))
     ("dgl"   . ,(user-dev "/guile"))

     ("user_full_name"    . ,user-full-name)
     ("user_mail_address" . ,user-mail-address)

     ;; used by gps, gpl
     ("remotes" . ,(string-join (list "origin" "gitlab")
                                list-separator))

     ;; `guix edit ...' reads $VISUAL and/or $EDITOR environment variables
     ("EDITOR" . "g") ;; The 'guix' branch of Spacemacs

     ;; My own scripts and guix-home profile take precedence over $PATH.
     ("PATH" . ,((comp
                  (lambda (lst) (string-join lst list-separator))
                  (lambda (lst) (append lst (list "$PATH" "/usr/local/bin")))
                  (partial map user-home))
                 (list scm-bin-dirpath bin-dirpath
                       ;; rga: ripgrep, plus search in pdf, E-Books, Office
                       ;; docs, zip, tar.gz, etc.
                       ;; See https://github.com/phiresky/ripgrep-all
                       ;; "user-home-relative/path/to/ripgrep_all"
                       ))))))
(testsymb 'environment-vars)

(define-public (environment-vars-edge-ecke list-separator)
  ((comp
    #;(lambda (v) (format #t "~a 0\n" m) v))
   `(
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
     ;; ("JAVA_HOME" . ,(string-append "/usr/lib/jvm/"
     ;;                             ;; "java-8-openjdk-amd64"
     ;;                                "java-11-openjdk-amd64"))

     ;; Setting the locale correctly:
     ;; https://systemcrafters.cc/craft-your-system-with-guix/installing-the-package-manager/#setting-the-locale-correctly
     ;; When 'setlocale: LC_ALL: cannot change locale'
     ;; ("GUIX_LOCPATH" . ,(user-home "/.guix-profile/lib/locale"))

     ;; needed by `help`; e.g. `help expand`
     ("BROWSER" . "firefox")

     ;; open man-pages in neovim
     ("MANPAGER" . "nvim +Man!")

     ;; for `flatpak run ...`
     ("XDG_DATA_DIRS" . ,((comp
                           (lambda (lst) (string-join lst list-separator)))
                          (list
                           (user-home "/.local/share/flatpak/exports/share")
                           "/var/lib/flatpak/exports/share"
                           (getenv "XDG_DATA_DIRS"))))

     ("dec"   . ,(user-home "/dec"))
     ("der"   . ,(user-home "/der"))
     ;; guile / guix load-path
     ("glp"  . ,((comp
                  (lambda (lst) (string-join lst list-separator)))
                 (append
                  (map user-dotf
                       (list
                        "/guix/common"
                        "/guix/home/common"
                        "/guix/systems/common"
                        "/guix/home"
                        "/guix/systems"
                        ))
                  (map user-dev
                       (list
                        "/guile"
                        "/guile-git"
                        "/nonguix"
                        ;; "/andrew-rde/src"
                        ))
                  (list (str dgxp "/src"))
                  (list dgx)
                  ))))))
(testsymb 'environment-vars-edge-ecke)

(define-public (environment-variables-service environment-vars)
  (simple-service
   'environment-variables-service
   home-environment-variables-service-type
   environment-vars))
(testsymb 'environment-variables-service)

;;; TODO add:
;;;   'Practicalli Clojure CLI Config'
;;;   https://github.com/practicalli/clojure-cli-config/
;;; to the projects, then:
;;    mkcd $XDG_CONFIG_HOME/clojure/ && git fetch --tags origin
;;; Checkout the latest tag:
;;    git checkout $(git tag --sort=-creatordate | head -n 1)
;;
;;  # when the tags are named v1.0.1, v1.0.2 etc.
;;  # git checkout $(git tag --sort=-v:refname | head -n 1)

(define (projects)
  (list
   (cons "/dec" (list
                 "/clj-time"
                 "/cljplot"
                 "/corona_cases"
                 "/fdk"
                 "/monad_koans"
                 "/morse"
                 "/utils"
                 "/cheatsheet"
                 "/zark"
                 ))
   #;
   (cons "/der" (list
                 "/heroku-buildpack-racket"
                 "/racket-koans"
                 "/search-notes"
                 ;; "/vesmir" ;; is in the projects-heroku list
                 ))
   (cons
    "/dev"
    (append
;;;   cp -r <repo-local-checkout> $dev
;;;   cd $dev/<repo-name>
;;;   git remote add origin <repo-url>
;;; The repo-url can be obtained from `guix describe --format=channels`
;;;   git fetch --tags origin master
     (list
      (list "/guix" "https://git.savannah.gnu.org/git/guix.git")
      (list "/nonguix" "https://gitlab.com/nonguix/nonguix"))
     (list
      (list "/elpa-mirror.d12frosted"
            "https://github.com/d12frosted/elpa-mirror")
      "/blog"
      "/copy-sexp"
      "/dotfiles"
      "/guix-packages"
      "/jump-last"
      "/kill-buffers"
      "/tweaks"
      "/notes"
      "/farmhouse-light-mod-theme"
      (list "/guile" "https://git.savannah.gnu.org/git/guix.git")
      (list "/guile-git" "https://gitlab.com/guile-git/guile-git.git"))))))
(testsymb 'projects)

;; wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
;; ln -s ~/dev/dotfiles/.lein
(define (obtain-and-setup dest-dir repo-orig)
  "TODO See https://gitlab.com/guile-git/guile-git.git
Guile bindings to libgit2, to manipulate repositories of the Git."
  (let* [(repo (if (list? repo-orig)
                   (cadr repo-orig)
                   repo-orig))
         (dest-dir-repo (if (list? repo-orig)
                            (str home dest-dir (car repo-orig))
                            (str home dest-dir repo)))]
    (unless (access? dest-dir-repo F_OK)
      (let* [(repo-url (if (url? repo)
                           repo
                           (str gitlab repo)))]
        (map (partial apply exec-system*)
             (list
              (list "git" "clone" "--origin=gitlab" repo-url dest-dir-repo)
              (list "git" (str "--git-dir=" dest-dir-repo "/.git") "remote add github"
                    (str github repo))))))))

(define (projects-heroku)
  (list
   (cons "/der" (list
                 ;; "/pictures"
                 ;; "/covid-survey"
                 "/vesmir"
                 ;; "/tetris"
                 "/vojto"))))

(define (obtain-and-setup-heroku dest-dir repo)
  (let* [(dest-dir-repo (str home dest-dir repo))]
    (unless (access? dest-dir-repo F_OK)
      (let [(repo-url (if #f ; (url? repo)
                          repo
                          (str "https://git.heroku.com/" repo ".git")))]
        (format #t "~a TODO clone ~a to ~a\n" m repo-url dest-dir-repo)
        #;
        (git-clone "--origin=vojto" repo-url dest-dir-repo)))))

(define-public (install-all-projects)
  ;; (format #t "~a projects to install: ~a\n" m (length (projects)))
  (map (lambda (project)
         ;; (format #t "~a project: ~a\n" m project)
         (let [(dest-dir (car project))]
           (map (partial obtain-and-setup dest-dir) (cdr project))))
       (projects))
  #;
  (map (lambda (project)
         (let ((dest-dir (car project)))
           (map (partial obtain-and-setup-heroku dest-dir) (cdr project))))
       (projects-heroku)))
(testsymb 'install-all-projects)

(define-public (home-env-services list-separator)
  ((comp
    (partial append (non-env-var-services))
    ;; (lambda (v)
    ;;   ;; (map (partial format #t "~a 1:\n~a\n" m) v)
    ;;   ;; (format #t "~a 1 (length v) : ~a\n" m (length v))
    ;;   v)
    list
    environment-variables-service
    (partial append (environment-vars-edge-ecke list-separator))
    (partial append (environment-vars           list-separator))
    ;; (lambda (v) (format #t "~a 0:\n~a\n" m v) v)
    )
   (list)))
(testsymb 'home-env-services)

(define-public (home-env-edge-ecke list-separator)
  (let* [(m (format #f "~a [home-env-edge-ecke]" m))]
    ;; (format #t "~a Starting…\n" m)
    (let* [(home-env-record
            (home-environment
              ;; (packages ...) replaced by $dotf/guix/profile-manifest.scm
              ;; (packages (home-packages-to-install))
              (services
               (home-env-services list-separator))))]
      ;; (format #t "~a done. type: ~a\n" m (test-type home-env-record))
      home-env-record)))
(testsymb 'home-env-edge-ecke)

(module-evaluated)
