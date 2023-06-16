;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

#|
# To prevent incorrect values in the ~/.guix-home/setup-environment (e.g.
# XDG_DATA_DIRS), reset environment variables to their default values by
# sourcing the default bash profile and run `guix home ...` command from bash:
source /etc/profile && dxh=~/dev/dotfiles/guix/home
guix home --allow-downgrades -L $dxh reconfigure $dxh/home-config-ecke.scm
# -L --load-path

# The tilda `~' is only expanded by shells when it's the first character of a
# command-line argument. Use $HOME instead

guix shell --development guix help2man git strace --pure
./pre-inst-env guix repl

;; see 'include', which unlike 'load', works within nested lexical contexts
;; can't use the `~'
(load "/home/bost/dev/dotfiles/guix/home/home-config-ecke.scm")

TODO see https://github.com/daviwil/dotfiles/tree/guix-home
|#
;; (format #t "[home-config-ecke] evaluating module ...\n")

(define-module (home-config-ecke)
  #:use-module ((common settings) #:prefix hs:)
  #:use-module (utils)
  #:use-module (memo)

  ;; fix the 'error: leiningen: unknown package', but it doesn't work
  #:use-module (nongnu packages clojure)

  ;; the code of this module comes in via the 'bost' channel
  ;; #:use-module (bost utils)

  #:use-module ((fs-utils) #:prefix hf:)
  ;; #:use-module ((cfg packages all-new) #:prefix hp:)
  #:use-module ((cfg packages all) #:prefix hp:)
  ;; #:use-module (cfg mcron)
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
  ;; first take remove delete-duplicates append-map etc.
  #:use-module (srfi srfi-1)
  ;; pretty-print
  ;; #:use-module (ice-9 pretty-print)

  ;; the https://issues.guix.gnu.org/51359 has not been merged yet
  ;; home-git-service-type
  ;; #:use-module (gnu home services version-control)
  )

(define m (module-name-for-logging))
;; (format #t "~a evaluating module ...\n" m)

(define indent "")
(define indent-inc "   ")

(define (environment-vars list-separator)
  `(
    ;; Warn about deprecated Guile features
    ("GUILE_WARN_DEPRECATED" . "detailed")
    ;; CC (or maybe CMAKE_C_COMPILER) is needed for: npm install --global heroku
    ("CC" . ,(user-home "/.guix-home/profile/bin/gcc"))

    ;; rga: ripgrep, plus search in pdf, E-Books, Office docs, zip, tar.gz, etc.
    ;; See https://github.com/phiresky/ripgrep-all
    ;; ("PATH" . ,(string-join (user-home "/bin/ripgrep_all") path))

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

    ;; TODO move CORONA_ENV_TYPE and REPL_USER to .envrc
    ;; see also $dec/corona_cases/.env and $dec/corona_cases/.heroku-local.env
    ("CORONA_ENV_TYPE" . "devel")
    ("REPL_USER" . ,user)

    ;; needed by `help`; e.g. `help expand`
    ("BROWSER" . "firefox")

    ;; for `flatpak run ...`
    ("XDG_DATA_DIRS" . ,(string-join
                         (list
                          (user-home "/.local/share/flatpak/exports/share")
                          "/var/lib/flatpak/exports/share"
                          (getenv "XDG_DATA_DIRS"))))

    ("cores" . "22") ;; for --cores=$cores; see `jobs=$[$(nproc) * 95 / 100]'
    ("dev"   . ,hf:dev)
    ("dec"   . ,(user-home "/dec"))
    ("der"   . ,(user-home "/der"))
    ("bin"   . ,(user-home hf:bin-dirpath))
    ("cheat" . ,(str hf:dev "/cheat"))
    ("dotf"  . ,(str hf:dev "/dotfiles"))
    ("dgx"   . ,(str hf:dev "/guix"))
    ("dgxp"  . ,(str hf:dev "/guix-packages"))
    ("dgl"   . ,(str hf:dev "/guile"))

    ("user_full_name"    . ,hs:user-full-name)
    ("user_mail_address" . ,hs:user-mail-address)

    ;; used by gps, gpl
    ("remotes" . ,(string-join (list "origin" "gitlab")
                               list-separator))

    ;; `guix edit ...' reads $VISUAL and/or $EDITOR environment variables
    ("EDITOR" . "e") ;; which "e": /home/bost/scm-bin/e

    ;; My own scripts and guix-home profile take precedence over $PATH.
    ("PATH" . ,(string-join (list (str home hf:scm-bin-dirpath)
                                  (str home hf:bin-dirpath)
;;; The paths to bin and sbin for guix-home profile are inserted here.
                                  "$PATH"
                                  "/usr/local/bin"
;;; TODO put ~/.npm-packages on PATH only if npm, i.e. node is installed
;;; See also ~/.npm, ~/.npmrc, ~/node_modules
                                  #;(str home "/.npm-packages"))
                            list-separator))))
(testsymb 'environment-vars)

(define environment-variables-service
  (simple-service
   'environment-variables-service
   home-environment-variables-service-type
   (environment-vars hf:list-separator-bash)))

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

(format #t "~a obtaining projects ... " m)
;; See https://gitlab.com/guile-git/guile-git.git
;; Guile bindings to libgit2, to manipulate repositories of the Git.

(define projects
  (list
;;    (cons "/dec" (list "/corona_cases" "/fdk" "/monad_koans"
;;                       "/morse" "/utils" "/clj-time" "/cljplot"
;;                       "/zark"))
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
;;                  "/notes" "/dotfiles"))
   ))

;; wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
;; ln -s ~/dev/dotfiles/.lein
(define (obtain-and-setup dest-dir repo)
  (let* [(gitlab "git@gitlab.com:rostislav.svoboda")
         (github "git@github.com:Bost")
         (dest-dir-repo (str home dest-dir repo))
         (repo-url (if #f ; (url? repo)
                       repo
                       (str gitlab repo)))]
    (gcl "--origin=gitlab" repo-url dest-dir-repo)
    (exec-system*
     "git" (str "--git-dir=" dest-dir-repo "/.git") "remote add github"
     (str github repo))))

;; Existing projects won't be overridden
;; (map (lambda (project)
;;        (let ((dest-dir (car project)))
;;          (map (partial obtain-and-setup dest-dir) (cdr project))))
;;      projects)
(format #t "done\n")

(format #t "~a obtaining projects-heroku ... " m)
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
         (dest-dir-repo (str home dest-dir repo))
         (repo-url (if #f ; (url? repo)
                       repo
                       (str heroku repo ".git"))))
    (gcl "--origin=vojto" repo-url dest-dir-repo)))

;; Existing projects won't be overridden
;; (map (lambda (project)
;;        (let ((dest-dir (car project)))
;;          (map (partial obtain-and-setup-heroku dest-dir) (cdr project))))
;;      projects-heroku)
(format #t "done\n")

(define (shell-config-file shell name content)
  (plain-file
   name
   (str
    "\n" "#### home-" shell "-configuration -> " name ": begin"
    "\n"
    content
    "\n"
    "\n" "#### home-" shell "-configuration -> " name ": end")))

(define (bash-config-file name content)
  (shell-config-file "bash" name content))

;; (begin
;;   ;; fish-config-base and fish-config-dotfiles are also defined in the (srvc fish)
;;   (define* (fish-config-base #:rest args)
;;     "(fish-config-base) ; => \".config/fish\""
;;     (apply str (basename xdg-config-home) "/fish" args))

;;   (define* (fish-config-dotfiles #:rest args)
;;     "(fish-config-dotfiles) ; => \"/home/bost/dev/dotfiles/.config/fish\"
;; Note:
;; (format #t \"~a\" \"foo\") doesn't work"
;;     (apply str (hf:dotfiles-home) "/" (fish-config-base) args))

;; ;;; TODO The (copy-file ...) is not an atomic operation, i.e. it's not undone
;; ;;; when the 'guix home reconfigure ...' fails or is interrupted.
;; ;;; Can't use `local-file' or `mixed-text-file' or something similar since the
;; ;;; `fish_variables' must be editable
;;   (let* [(filepath "/fish_variables")
;;          (src (fish-config-dotfiles filepath))
;;          (dst (user-home "/" (fish-config-base filepath)))
;;          (dstdir (dirname dst))]
;;     (unless (file-exists? dstdir)
;;       (let [(indent (str indent indent-inc))]
;;         (format #t "~a(mkdir ~a) ... " indent src dstdir)
;;         (let ((retval (mkdir dstdir)))
;;           (format #t "retval: ~a\n" retval)
;;           ;; The value of 'retval' is '#<unspecified>'
;; ;;; TODO continuation: executing the block only if the dstdir was created.
;;           retval)))
;; ;;; TODO is this sexp is not executed because of lazy-evaluation?
;;     (let [(indent (str indent indent-inc))]
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
        (str
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

   (service dirs-service-type)
   fish-service
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
(testsymb 'my=services)

;; Note: `home-environment' is (lazily?) evaluated as a last command
;; (let ((he (home-environment ...))) (format #t "Should be last\n") he)
(define (home-env)
  (home-environment
;;; TODO see also the xfce4 chromium launcher -> command
;;; /home/bost/.guix-profile/bin/chromium %U

;;; TODO see [PATCH] services: Add udev-rules-service helper.
;;; https://issues.guix.gnu.org/40454

;;; TODO home-git-configuration

   (packages (hp:packages-to-install))
   (services my=services)))
(testsymb 'home-env)

;; TODO put home-config-ecke and system-configuration in one file
;; (if (getenv "RUNNING_GUIX_HOME") home system)

(when (home-ecke-config)
  (home-env))
