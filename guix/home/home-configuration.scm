;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

#|
# To prevent incorrect values in the ~/.guix-home/setup-environment (e.g.
# XDG_DATA_DIRS), reset environment variables to their default values by
# sourcing the default bash profile and run `guix home ...` command from bash:
$ source /etc/profile
$ guix home --allow-downgrades --load-path=$dotf/guix/home \
       reconfigure $dotf/guix/home/home-configuration.scm

# The tilda `~' is only expanded by shells when it's the first character of a
# command-line argument. Use $HOME instead

guix shell --development guix help2man git strace --pure
./pre-inst-env guix repl

(getcwd)
(add-to-load-path
 (hu:str hu:home "/dev/guix"))
(add-to-load-path
 (hu:str hu:home "/dev/dotfiles.dev/guix/home"))
;; see 'include', which unlike 'load', also works within nested lexical contexts
;; can't use the `~'
,load "/home/bost/dev/dotfiles.dev/guix/home/home-configuration.scm"
(load "/home/bost/dev/dotfiles.dev/guix/home/home-configuration.scm")

TODO see https://github.com/daviwil/dotfiles/tree/guix-home
|#
(format #t "[home-configuration] evaluating module ...\n")

(define-module (home-configuration)
  ;; #:use-module (cfg packages-new)
  #:use-module ((common settings) #:prefix hs:)
  #:use-module ((utils) #:prefix hu:)
  #:use-module ((fs-utils) #:prefix hf:)
  #:use-module ((cfg packages) #:prefix hp:)
  ;; #:use-module (cfg mcron)
  #:use-module (srvc my=fish)
  #:use-module ((srvc my=simple-services) #:prefix srvc:)
  ;; See service-file -> with-imported-modules
  #:use-module (scm-bin gcl)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  ;; program-file local-file
  #:use-module (guix gexp)
  #:use-module (gnu home services shells)
  ;; simple-service
  #:use-module (gnu home services)
  ;; take remove delete-duplicates append-map etc.
  #:use-module (srfi srfi-1)
  ;; pretty-print
  ;; #:use-module (ice-9 pretty-print)

  ;; the https://issues.guix.gnu.org/51359 has not been merged yet
  ;; home-git-service-type
  ;; #:use-module (gnu home services version-control)
  )

(define indent "")
(define indent-inc "   ")

(define (environment-vars list-separator)
  `(
    ;; Warn about deprecated Guile features
    ("GUILE_WARN_DEPRECATED" . "detailed")
    ;; CC (or maybe CMAKE_C_COMPILER) is needed for: npm install --global heroku
    ("CC" . ,(hu:user-home "/.guix-home/profile/bin/gcc"))

    ;; rga: ripgrep, plus search in pdf, E-Books, Office docs, zip, tar.gz, etc.
    ;; See https://github.com/phiresky/ripgrep-all
    ;; ("PATH" . ,(string-join (hu:user-home "/bin/ripgrep_all") path))

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
    ;; ("GUIX_LOCPATH" . ,(hu:user-home "/.guix-profile/lib/locale"))

    ;; TODO move CORONA_ENV_TYPE and REPL_USER to .envrc
    ;; see also $dec/corona_cases/.env and $dec/corona_cases/.heroku-local.env
    ("CORONA_ENV_TYPE" . "devel")
    ("REPL_USER" . ,hu:user)

    ;; needed by `help`; e.g. `help expand`
    ("BROWSER" . "firefox")

    ;; for `flatpak run ...`
    ("XDG_DATA_DIRS" . ,(string-join
                         (list
                          (hu:user-home "/.local/share/flatpak/exports/share")
                          "/var/lib/flatpak/exports/share"
                          (getenv "XDG_DATA_DIRS"))))

    ("dev"   . ,hf:dev)
    ("dec"   . ,(hu:user-home "/dec"))
    ("der"   . ,(hu:user-home "/der"))
    ("bin"   . ,(hu:user-home hf:bin-dirpath))
    ("cheat" . ,(hu:str hf:dev "/cheat"))
    ("dotf"  . ,(hu:str hf:dev "/dotfiles"))

    ("user_full_name"    . ,hs:user-full-name)
    ("user_mail_address" . ,hs:user-mail-address)

    ;; used by ghog glog
    ("remotes" . ,(string-join (list "origin" "gitlab")
                               list-separator))

    ;; `guix edit ...' reads $VISUAL and/or $EDITOR environment variables
    ("EDITOR" . "e") ;; which "e": /home/bost/scm-bin/e
    ;; TODO test if the library exists:
    ;;   test -e $LDP && set --export LD_PRELOAD $LDP
    ;; ("LD_PRELOAD" . "/usr/lib/x86_64-linux-gnu/libgtk3-nocsd.so.0")

    ;; My own scripts and guix-home profile take precedence over $PATH.
    ("PATH" . ,(string-join (list (hu:str hu:home hf:scm-bin-dirpath)
                                  (hu:str hu:home hf:bin-dirpath)
;;; The paths to bin and sbin for guix-home profile are inserted here.
                                  "$PATH"
                                  "/usr/local/bin"
;;; TODO put ~/.npm-packages on PATH only if npm, i.e. node is installed
;;; See also ~/.npm, ~/.npmrc, ~/node_modules
                                  #;(hu:str hu:home "/.npm-packages"))
                            list-separator))))

(define environment-variables-service
  (simple-service
   'environment-variables-service
   home-environment-variables-service-type
   (environment-vars hf:list-separator-bash)))

(define (read-module relative-path name)
  "TODO use monad"
  (let* [(iindent (hu:str indent indent-inc))
         (name-scm (hu:str name ".scm"))
         (filepath (hf:dotfiles-home "/guix/home" relative-path "/" name-scm))]
    (format #t "~aread-module: ~a ... " iindent name)
    (let ((sf (scheme-file name-scm
                           (sexp->gexp
                            (call-with-input-file filepath hu:read-all-sexprs))
                           #:splice? #t)))
      (format #t "done\n")
      sf)))

(format #t "~a:\n" "Pre-calculating modules")
(define module-utils (read-module "" "utils"))
(define module-common-settings (read-module "/common" "settings"))
(define module-ls (read-module hf:scm-bin-dirpath "ls"))
(define module-chmod (read-module hf:scm-bin-dirpath "chmod"))
(define module-search-notes (read-module hf:scm-bin-dirpath "search-notes"))
(format #t "done\n")

(define* (service-file #:key
                       program-name desc scheme-file-name module-name
                       chmod-params files)
  "The priority is 1. module-name, 2. scheme-file-name, 3. program-name

TODO The `search-notes' program should read a `search-space-file' containing
a list of files to search through.

Example:
    chmod --recursive u=rwx,g=rwx,o=rwx /path/to/dir
"
  `(,(hu:str hf:scm-bin-dirname "/" program-name)
    ,(program-file
      (cond
       ((equal? scheme-file-name "chmod")
        (hu:str "chmod-plus-" chmod-params))
       ((equal? scheme-file-name "search-notes")
        (hu:str "search-notes-" program-name))
       (#t
        desc))
      ;; TODO clarify if source-module-closure is needed only for imports of
      ;; guix modules?
      (let* ((symb-string (or scheme-file-name program-name))
             (symb (or module-name
                       (string->symbol symb-string)))
             (main-call
              (remove unspecified?
                      `(main ,(cond
                               ((equal? scheme-file-name "chmod")
                                chmod-params)
                               ((equal? scheme-file-name "search-notes")
                                files))
                             (command-line)))))
        (with-imported-modules
            (remove
             unspecified?
             `(((utils) => ,module-utils)
               ((common settings) => ,module-common-settings)
               ;; module-search-notes
               ;; 'ls' is needed only for 'lf.scm'
               ,(cond
                 ((equal? symb-string "lf")
                  `((scm-bin ls) => ,module-ls))

                 ((equal? scheme-file-name "chmod")
                  `((scm-bin ,symb) => ,module-chmod))

                 ((equal? scheme-file-name "search-notes")
                  `((scm-bin ,symb) => ,module-search-notes))

                 (#t
                  `((scm-bin ,symb) => ,(read-module hf:scm-bin-dirpath
                                                     symb-string))))))
          #~(begin
              (use-modules (scm-bin #$symb))
              #$main-call))))))

;; xfce4-keyboard: repeat-delay 160 repeat-speed 60

;; "copying files"
;; there should be a service type to place particular files (or file-like
;; objects) to a target destination

;; extend home-activation-service-type or home-run-on-first-login-service-type
;; to run some scripts, required to be idempotent though.

;; use home-files-service-type for copying configurations
;; home-files-service-type example:
;; (services
;;  ...
;;  (list
;;   (simple-service 'dotfiles-installation
;;                   home-files-service-type
;;                   `((".config/zsh" ;; destination
;;                      ,(local-file
;;                        "/home/foobar/etc/zsh/.config/zsh" ;; source file/directory
;;                        "zsh-config"
;;                        ;; #t to copy directory
;;                        #:recursive? #t))))))

(format #t "~a ... " "Obtaining projects")
;; See https://gitlab.com/guile-git/guile-git.git
;; Guile bindings to libgit2, to manipulate repositories of the Git.
(define projects (list))
;; (define projects
;;   (list
;;    (cons "/dec" (list "/corona_cases" "/fdk" "/monad_koans"
;;                       "/morse" "/utils" "/clj-time" "/cljplot"))
;;    (cons "/der" (list "/search-notes" "/racket-koans"
;;                       ;; "/vesmir" is in the projects-heroku list
;;                       "/heroku-buildpack-racket"))
;;    (cons "/dev" (list
;;                  "/guix-packages" ;; "/guix"
;;                  "/copy-sexp" "/kill-buffers" "/jump-last"
;; ;;; use the local guix repo-checkout instead of git.savannah.gnu.org:
;; ;;; set latest (ls --sort=time --almost-all ~/.cache/guix/checkouts/ | head -1)
;; ;;; cd ~/.cache/guix/checkouts/$latest
;;             ;;;; (cons "/guix" "https://git.savannah.gnu.org/git/guix.git")
;; ;;; ... then
;; ;;;   git remote rename origin checkout
;; ;;;   git remote add origin https://git.savannah.gnu.org/git/guix.git
;; ;;;   git fetch --tags origin
;;             ;;;; (cons "/guile" "https://git.savannah.gnu.org/git/guix.git")
;;                  "/notes" "/dotfiles"))))

;; wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
;; ln -s ~/dev/dotfiles/.lein
(define (obtain-and-setup dest-dir repo)
  (let* [(gitlab "git@gitlab.com:rostislav.svoboda")
         (github "git@github.com:Bost")
         (dest-dir-repo (hu:str hu:home dest-dir repo))
         (repo-url (if #f ; (url? repo)
                       repo
                       (hu:str gitlab repo)))]
    (gcl "--origin=gitlab" repo-url dest-dir-repo)
    (exec-system*
     "git" (hu:str "--git-dir=" dest-dir-repo "/.git") "remote add github"
     (hu:str github repo))))

;; Existing projects won't be overridden
;; (map (lambda (project)
;;        (let ((dest-dir (car project)))
;;          (map (hu:partial obtain-and-setup dest-dir) (cdr project))))
;;      projects)
(format #t "done\n")

(format #t "~a ... " "Obtaining projects-heroku")
(define projects-heroku
  (list
   (cons "/der" (list
                 ;; "/pictures"
                 ;; "/covid-survey"
                 "/vesmir"
                 ;; "/tetris"
                 "/vojto"))))

(define (obtain-and-setup-heroku dest-dir repo)
  (let* ((heroku "https://git.heroku.com/")
         (dest-dir-repo (hu:str hu:home dest-dir repo))
         (repo-url (if #f ; (url? repo)
                       repo
                       (hu:str heroku repo ".git"))))
    (gcl "--origin=vojto" repo-url dest-dir-repo)))

;; Existing projects won't be overridden
;; (map (lambda (project)
;;        (let ((dest-dir (car project)))
;;          (map (hu:partial obtain-and-setup-heroku dest-dir) (cdr project))))
;;      projects-heroku)
(format #t "done\n")

(define (shell-config-file shell name content)
  (plain-file
   name
   (hu:str
    "\n" "#### home-" shell "-configuration -> " name ": begin"
    "\n"
    content
    "\n"
    "\n" "#### home-" shell "-configuration -> " name ": end")))

(define (bash-config-file name content)
  (shell-config-file "bash" name content))

;; (begin
;;   ;; fish-config-base and fish-config-dotfiles are also defined in the my=fish
;;   (define* (fish-config-base #:rest args)
;;     "(fish-config-base) ; => \".config/fish\""
;;     (apply hu:str (basename xdg-config-home) "/fish" args))

;;   (define* (fish-config-dotfiles #:rest args)
;;     "(fish-config-dotfiles) ; => \"/home/bost/dev/dotfiles/.config/fish\"
;; Note:
;; (format #t \"~a\" \"foo\") doesn't work"
;;     (apply hu:str (hf:dotfiles-home) "/" (fish-config-base) args))

;; ;;; TODO The (copy-file ...) is not an atomic operation, i.e. it's not undone
;; ;;; when the 'guix home reconfigure ...' fails or is interrupted.
;; ;;; Can't use `local-file' or `mixed-text-file' or something similar since the
;; ;;; `fish_variables' must be editable
;;   (let* [(filepath "/fish_variables")
;;          (src (fish-config-dotfiles filepath))
;;          (dst (hu:user-home "/" (fish-config-base filepath)))
;;          (dstdir (dirname dst))]
;;     (unless (file-exists? dstdir)
;;       (let [(indent (hu:str indent indent-inc))]
;;         (format #t "~a(mkdir ~a) ... " indent src dstdir)
;;         (let ((retval (mkdir dstdir)))
;;           (format #t "retval: ~a\n" retval)
;;           ;; The value of 'retval' is '#<unspecified>'
;; ;;; TODO continuation: executing the block only if the dstdir was created.
;;           retval)))
;; ;;; TODO is this sexp is not executed because of lazy-evaluation?
;;     (let [(indent (hu:str indent indent-inc))]
;;       (format #t "~a(copy-file ~a ~a) ... " indent src dst)
;;       (let ((retval (copy-file src dst)))
;;         (format #t "retval: ~a\n" retval)
;;         ;; The value of 'retval' is '#<unspecified>'
;;         retval))
;; ;;; Just changing ownership and permissions of `fish_variables' doesn't work:

;;     ;; (begin
;;     ;;   ;; .rw-r--r-- bost users fish_variables
;;     ;;   (format #t "(chown ~a ~a ~a)\n" dst (getuid) (getgid))
;;     ;;   (chown dst (getuid) (getgid))
;;     ;;   ;; .rw-r--r-- fish_variables
;;     ;;   (format #t "(chmod ~a ~a)\n" dst #o644)
;;     ;;   (chmod dst #o644))
;;     ))

(define scheme-files-service
  ((compose
    (hu:partial simple-service 'scheme-files-service home-files-service-type)
    (hu:partial
     append
     (cond
      [(hu:home-ecke-config)
       (list
        (service-file #:program-name "e" #:desc "emacs-launcher"
                      #:scheme-file-name "emacs-launcher")
        (service-file #:program-name "s" #:desc "spguimacs-launcher"
                      #:scheme-file-name "spguimacs-launcher")
;;; TODO `gui' should do `cd ~/dev/guix'
;;; TODO `guixg' should do `git pull --rebase' (preferably from a local guix
;;; checkout)
;;; TODO crc should search in the $dec
        (service-file #:program-name "crc"  #:files "clojure"
                      #:scheme-file-name "search-notes")
;;; TODO cre should also search in the ~/.emacs.d/, ~/.spacemacs, kill-buffes
;;; and my=tweaks, farmhouse-light-mod
        (service-file #:program-name "cre"  #:files "vim|emacs|org_mode"
                      #:scheme-file-name "search-notes")
        (service-file #:program-name "crep" #:files ".*"
                      #:scheme-file-name "search-notes")
        (service-file #:program-name "crf"  #:files "find_and_grep"
                      #:scheme-file-name "search-notes")
;;; TODO crg should also search in the $dotf/guix/
        (service-file #:program-name "crg"  #:files "guix|guile"
                      #:scheme-file-name "search-notes")
;;; TODO crgi should also search in the output of `git config --get',
;;; ~/.gitconfig, etc.
        (service-file #:program-name "crgi" #:files "git"
                      #:scheme-file-name "search-notes")
;;; TODO crl should search in the $dotf/.config/fish .bashrc, .bash_profile (and
;;; other profile files), etc.
        (service-file #:program-name "crl"
                      #:files "guix|shells|linux|android"
                      #:scheme-file-name "search-notes")
;;; TODO crr should also search in the $der
        (service-file #:program-name "crr"  #:files "racket"
                      #:scheme-file-name "search-notes")
;;; TODO crs should be like crl
        (service-file #:program-name "crs"  #:files "shells"
                      #:scheme-file-name "search-notes")
        (service-file #:program-name "cru"  #:files "utf8"
                      #:scheme-file-name "search-notes")
        (service-file #:program-name "qemu-vm" #:desc "qemu-virt-machine")
        (service-file #:program-name "spag"
                      #:desc "spacemacs-git-fetch-rebase")
        )]
      [#t
       ;; empty list
       (list)])))
   (list
    (service-file #:program-name "prw"  #:chmod-params "rw"
                  #:scheme-file-name "chmod")
    (service-file #:program-name "px"   #:chmod-params "x"
                  #:scheme-file-name "chmod")
    (service-file #:program-name "ext"     #:desc "extract-uncompress"
                  #:scheme-file-name "extract")
    (service-file #:program-name "c"       #:desc "batcat"
                  #:scheme-file-name "bat")
    (service-file #:program-name "f"       #:desc "find-alternative")
    (service-file #:program-name "gcl"     #:desc "git-clone")
    (service-file #:program-name "gre"     #:desc "git-remote")
    (service-file #:program-name "gfe"     #:desc "git-fetch")
    (service-file #:program-name "gco"     #:desc "git-checkout")
    (service-file #:program-name "gcod"
                  #:desc "git-checkout-previous-branch")
    (service-file #:program-name "gcom"
                  #:desc "git-checkout-master")
    (service-file #:program-name "gg"      #:desc "git-gui")
    (service-file #:program-name "ghog"
                  #:desc "git-push-to-remotes")
    (service-file #:program-name "gk"
                  #:desc "git-repository-browser")
    (service-file #:program-name "glo"
                  #:desc "git-fech-and-rebase-from-origin")
    (service-file #:program-name "gs"      #:desc "git-status")
    (service-file #:program-name "gtg"     #:desc "git-tag")
    (service-file #:program-name "l"
                  #:desc "list-directory-contents"
                  #:scheme-file-name "ls")
    (service-file #:program-name "lf"
                  #:desc "list-directory-contents-with-full-paths")
    )))

(define my=services
  (list
   ;; (service home-xsettingsd-service-type)
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
       (bash-config-file
        "bashrc"
        (hu:str
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
;;;       . "$GUIX_PROFILE/etc/profile"
;;;    i.e. `. ~/.guix-profile/etc/profile`
         "\n" "eval \"$(direnv hook bash)\""))

       (let* [(filename ".bashrc_additions")]
         ;; this should work too:
         ;; (local-file ".bashrc" (hf:fix-leading-dot ".bashrc"))
         (local-file
          (hf:dotfiles-home "/guix/home/" filename)
          (hf:fix-leading-dot filename)))))

     ;; List of file-like objects, which will be ADDED(!) to .bash_profile
     (bash-profile
      (list
       (bash-config-file
        "bash-profile"
        (hu:str
         "\n" "export HISTFILE=$XDG_CACHE_HOME/.bash_history"))
       ;; (local-file ".bashrc" "bash_profile") should work too
       ;; (local-file
       ;;  (hf:dotfiles-home "/.bash_profile_additions")
       ;;  ;; prevent "guix home: error: invalid name: `.bash_profile'"
       ;;  "bash_profile_additions")
       ))))

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

   my=fish-service
   environment-variables-service
   srvc:home-dir-cfg-srvc
   scheme-files-service
   ;; mcron-service

;;; https://github.com/babariviere/dotfiles/blob/1deae9e15250c86cc235bb7b6e69ea770af7b13a/baba/home/gaia.scm
;;; https://github.com/babariviere/dotfiles/blob/guix/baba/home/gaia.scm
;;; [WIP] home: Add home-git-service-type https://issues.guix.gnu.org/54293 is
;;; not pulled yed

   ;; (service home-git-service-type
   ;;          (home-git-configuration
   ;;           (config
   ;;            `((user
   ;;               ((name . ,hs:user-full-name)
   ;;                (email . ,hs:user-mail-address)
   ;;                #;(signingKey . "...")))
   ;;              (github
   ;;               ((user . "Bost")))
   ;;              (remote
   ;;               ((pushDefault . "origin")))
   ;;              #;(commit ((gpgSign . #t)))
   ;;              #;(tag ((gpgSign . #t)))))))
   ))

;; Note: `home-environment' is (lazily?) evaluated as a last command
;; (let ((he (home-environment ...))) (format #t "Should be last\n") he)
(define home-env
  (home-environment
;;; TODO why are the channels listed here???
;;; $ guix package --profile=/home/bost/.config/guix/current --list-installed
;;; guix     0321cee out /gnu/store/ada4wp2h2xqmrmz448xyp6nzli6drwsv-guix-0321ceef0
;;; nonguix  9563de3 out /gnu/store/1qz2whvn763yhxs5gdrsf9zqip3zspc2-nonguix
;;; babashka 31edde3 out /gnu/store/k64hd1q6gv3aa9r8arrdlaspzxy68444-babashka

;;; `guix package --list-profiles` doesn't know about / ignores the
;;; package-profile of the home-environment (~/.guix-home/profile/manifest)
;;; see also /run/current-system/profile

;;; $ guix package --search-paths --profile=~/.guix-home/profile -I | sort > /tmp/packages-guix-home.txt
;;; $ guix package --search-paths --profile=~/.guix-home/profile -I fish
;;; fish	3.5.1	out	/gnu/store/vj3kqlk3w7x6gqqb3qzl4jxq34xvy3q2-fish-3.5.1

;;; $ guix package --search-paths --profile=~/.guix-profile -I | sort > /tmp/packages-guix-profile.txt
;;; $ guix package --search-paths --profile=~/.guix-profile -I fish

;;; TODO see also the xfce4 chromium launcher -> command
;;; /home/bost/.guix-profile/bin/chromium %U


;;; TODO make it support inferior packages
;;; https://guix.gnu.org/manual/devel/en/html_node/Inferiors.html
;;; TODO packages should accept expressions like the -e, e.g.
;;;   guix package                        -e '(@ (bost packages maven) maven)'
;;;   guix package --install-from-expression='(@ (bost packages maven) maven)'
   (packages
;;; TODO following warning appears:
;;;     hint: Did you forget `(use-modules (gnu services))'?
;;; when using
;;;    (list (specification->package+output "hello"))
;;; instead of
;;;    (list hello) ;; hint need to add: #:use-module (gnu packages base) #| hello |#
    (map (compose identity list
;;; TODO what's the difference between specification->package+output and
;;; specification->package ?
                  specification->package+output)
         hp:packages-to-install))

;;; TODO see [PATCH] services: Add udev-rules-service helper.
;;; https://issues.guix.gnu.org/40454

   (services my=services)))

;; TODO put home-configuration and system-configuration in one file
;; (if (getenv "RUNNING_GUIX_HOME") home system)

(format #t "[home-configuration] module evaluated\n")

home-env
