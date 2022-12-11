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
 (str home "/dev/guix"))
(add-to-load-path
 (str home "/dev/dotfiles.dev/guix/home"))
;; see 'include', which unlike 'load', also works within nested lexical contexts
;; can't use the `~'
,load "/home/bost/dev/dotfiles.dev/guix/home/home-configuration.scm"
(load "/home/bost/dev/dotfiles.dev/guix/home/home-configuration.scm")

TODO see https://github.com/daviwil/dotfiles/tree/guix-home
|#

(define-module (home-configuration)
  ;; #:use-module (cfg packages-new)
  #:use-module (cfg packages)
  #:use-module (cfg spguimacs-packages)
  #:use-module (cfg mcron)
  ;; See service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (common settings)
  #:use-module (gcl)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix gexp)               #| program-file local-file |#
  #:use-module (gnu home services shells)
  #:use-module (gnu home services mcron) #| home-mcron-service-type |#
  #:use-module (gnu home services)       #| simple-service |#
  #:use-module (ice-9 ftw)               #| scandir |#
  ;; #:use-module (ice-9 string-fun)        #| string-replace-substring |#
  #:use-module (guix build utils)        #| invoke |#
  #| take remove delete-duplicates etc. |#
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages shells)     #| #$fish-foreign-env |#

  #:use-module (ice-9 pretty-print)      #| pretty-print |#
  ;; the https://issues.guix.gnu.org/51359 has not been merged yet
  #| home-git-service-type |#
  ;; #:use-module (gnu home services version-control)
  )

;; (format #t "~a... " "")
;; (format #t "done\n")

(define indent "")
(define indent-inc "   ")

(define home-games-config #f)

(define channels-scm-filepath (str (basename xdg-config-home) "/guix/channels.scm"))

(format #t "~a ... " "(define* (dotfiles-home ...) ...)")
(define* (dotfiles-home #:rest args)
  "Note:
(format #t \"~a\" \"foo\") doesn't work"
  (apply str home "/dev/dotfiles" args))
(format #t "done\n")

(define (fix-leading-dot filename)
  (string-replace filename "dot-" 0 1))

(format #t "~a ... " "(define* (any-local-file ...) ...)")
;;;      ...                      #:optional (<parameter-name> <default-value>)
(define* (any-local-file filepath #:optional (filename (basename filepath)))
  ;; 'local-file' is a macro and cannot be used by 'apply'

  (if (equal? "." (substring filename 0 1))
      ;; filename of the local-file can't start with '.'
      (local-file filepath (fix-leading-dot filename))
      (local-file filepath)))
(format #t "done\n")

(format #t "~a ... " "(define (local-dotfile path filename) ...)")
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
(format #t "done\n")

(format #t "~a ... " "fish-config-<stuff>")
(define* (fish-config-base #:rest args)
  (apply str (basename xdg-config-home) "/fish" args))

(define* (fish-config-dotfiles #:rest args)
  "Note:
(format #t \"~a\" \"foo\") doesn't work"
  (apply str (dotfiles-home) "/" (fish-config-base) args))
(format #t "done\n")

;; fish and bash separate elements of a list with a different separator
(define list-separator-bash ":")
(define list-separator-fish " ")
(define bin-dirpath "/bin")
(define sbin-dirpath "/sbin")
(define scm-bin-dirname "scm-bin")
(define scm-bin-dirpath (str "/" scm-bin-dirname))
(define dev (user-home "/dev"))

(format #t "~a ... " "(define (environment-vars ...) ...)")
(define (environment-vars list-separator)
  `(
    ;; CC (or maybe CMAKE_C_COMPILER) is needed for: npm install --global heroku
    ("CC" . ,(user-home "/.guix-home/profile/bin/gcc"))

    ;; rga: ripgrep, plus search in pdf, E-Books, Office docs, zip, tar.gz, etc.
    ;; See https://github.com/phiresky/ripgrep-all
    ;; ("PATH" . ,(string-join (usr-home "/bin/ripgrep_all") path))

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

    ("dev"   . ,dev)
    ("dec"   . ,(user-home "/dec"))
    ("der"   . ,(user-home "/der"))
    ("bin"   . ,(user-home bin-dirpath))
    ("cheat" . ,(str dev "/cheat"))
    ("dotf"  . ,(str dev "/dotfiles"))

    ("user_full_name"    . ,user-full-name)
    ("user_mail_address" . ,user-mail-address)

    ;; used by ghog glog
    ("remotes" . ,(string-join (list "origin" "gitlab")
                               list-separator))

    ;; `guix edit ...' reads $VISUAL and/or $EDITOR environment variables
    ("EDITOR" . "e") ;; which "e": /home/bost/scm-bin/e
    ;; TODO test if the library exists:
    ;;   test -e $LDP && set --export LD_PRELOAD $LDP
    ;; ("LD_PRELOAD" . "/usr/lib/x86_64-linux-gnu/libgtk3-nocsd.so.0")

    ;; My own scripts and guix-home profile take precedence over $PATH.
    ("PATH" . ,(string-join (list (str home scm-bin-dirpath)
                                  (str home bin-dirpath)
   ;; The paths to bin and sbin for guix-home profile are inserted here.
                                  "$PATH"
                                  "/usr/local/bin")
                            list-separator))))
(format #t "done\n")

;; fish and bash separate elements of a list with a different separator
(define list-separator-bash ":")
#;(define list-separator-fish " ") ;; not needed

(format #t "~a ... " "(define environment-variables-service ...)")
(define environment-variables-service
  (simple-service
   'environment-variables-service
   home-environment-variables-service-type
   (environment-vars list-separator-bash)))
(format #t "done\n")

(format #t "~a ... " "(define (read-module ...) ...)")
(define (read-module name)
  "TODO use monad"
  (let* [(iindent (str indent indent-inc))
         (name-scm (str name ".scm"))
         (filepath (dotfiles-home "/guix/home/" name-scm))]
    (format #t "~aread-module: ~a ... " iindent filepath)
    (let ((sf (scheme-file name-scm
                           (sexp->gexp
                            (call-with-input-file filepath read-all-sexprs))
                           #:splice? #t)))
      (format #t "done\n")
      sf)))
(format #t "done\n")

(format #t "~a ...\n" "(define module-utils ...)")
(define module-utils (read-module "utils"))
(format #t "done\n")

(format #t "~a ...\n" "(define module-ls ...)")
(define module-ls (read-module "ls"))
(format #t "done\n")

(format #t "~a ...\n" "(define module-spag ...)")
(define module-spag (read-module "spag"))
(format #t "done\n")

(format #t "~a ... " "(define* (service-file ...) ...)")
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
                                 ;; ls needed only for lf.scm
                                 ((ls)    => ,module-ls)
                                 ((,symb) => ,(read-module symb-string)))
                               #~(begin
                                   (use-modules (#$symb))
                                   (main (command-line))))))))
(format #t "done\n")

(format #t "~a ... " "(define* (search-notes ...) ...)")
(define* (search-notes #:key program-name files)
  "TODO The `search-notes' program should read a `search-space-file' containing a list
of files to search through."
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
(format #t "done\n")

(format #t "~a ... " "(define (append-fish-config-dir ...) ...)")
(define (append-fish-config-dir dir lst)
  (append
   `((,(fish-config-base dir)
      ,(local-file (fish-config-dotfiles dir)
                   #:recursive? #t)))
   lst))
(format #t "done\n")

(format #t "~a ... " "(define* (chmod-plus ...) ...)")
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
(format #t "done\n")

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

;; ;; See https://10years.guix.gnu.org/static/slides/05-wilson.org
;; (define (home-xsettingsd-files-service config)
;;   (list `(".config/xsettingsd/xsettingsd.conf"
;;           ,(local-file "xsettingsd.conf"))))

;; (define home-xsettingsd-service-type
;;   (service-type (name 'home-xsettingsd)
;;                 (extensions
;;                  (list (service-extension
;;                         home-files-service-type
;;                         home-xsettingsd-files-service)))
;;                 (default-value #f)
;;                 (description "Configures UI appearance settings for Xorg
;; sessions using the xsettingsd daemon.")))

(format #t "~a ... " "(define guix-channels-configuration ...)")
(define guix-channels-configuration
  (if home-games-config
      (list
       (local-dotfile "/" (str (basename xdg-config-home)
                               "/guix-gaming-channels/games.scm"))
       (let* [(lst (car (syntax->datum
                         (call-with-input-file channels-scm-filepath
                           (read-all read-syntax)))))]
         (call-with-values
             (lambda () (split-at lst (1- (length lst))))
           (lambda (fst snd)
             ((compose
               #;(lambda (sexp) (scheme-file "channels.scm" (sexp->gexp sexp)))
               (lambda (sexp)
                 (format #t "########## Creating plain-file ... \n")
                 (list
                  channels-scm-filepath
                  #;(scheme-file "channels.scm" (sexp->gexp sexp))
                  (local-file
                   (let* [(tmpfile (tmpnam))
                          (port
                           ;; create temporary file
                           #;(mkstemp! (string-copy "/tmp/myfile-XXXXXX"))
                           (open-output-file tmpfile))]
                     ;; save the channel configuration to a temporary file
                     (pretty-print sexp port)
                     (close-port port)
                     tmpfile)
                   "channels.scm")))
               (lambda (sexp) (append fst (list sexp) (list (car snd)))))
;;; https://raw.githubusercontent.com/wube/factorio-data/master/changelog.txt
;;; Use:
;;;     guix package --load-path=./ --install=factorio
;;; '--keep-failed' doesn't keep the binary in the /gnu/store when the sha256 is
;;; wrong
              '(channel
                (name 'guix-gaming-games)
                (url
                 #;"https://gitlab.com/rostislav.svoboda/games"
                 #;(string-append "file://" home "/dev/games")
                 "https://gitlab.com/guix-gaming-channels/games.git")
                ;; Enable signature verification:
                (introduction
                 (make-channel-introduction
                  "c23d64f1b8cc086659f8781b27ab6c7314c5cca5"
                  (openpgp-fingerprint
                   "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F")))))))))
      (list
       (local-dotfile "/" channels-scm-filepath))))
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

(define (fish-config-file name content)
  (shell-config-file "fish" name content))

(define home-dir-config-service
  ;; TODO add to home-dir-config: notes, rest of the $dotf/.emacs.d directory
  (let [(srvc-name 'home-dir-config-service)]
    (format #t "Running ~a ...\n" srvc-name)
    (let ((simple-srvc
           (simple-service
            srvc-name home-files-service-type
            ((compose
              (partial append guix-channels-configuration)
              (partial append
                       (remove
                        unspecified-or-empty-or-false?
                        (list
                         (local-dotfile "/" ".guile") ;; used by `guix repl'
                         (local-dotfile "/" ".gitconfig")
                         (local-dotfile "/" ".spacemacs")
                         (local-dotfile "/" ".spguimacs")
                         (local-dotfile "/guix/home/" "local-stuff.fish"))))
              (partial append
;;; This can't be used:
;;;                    `((".emacs.d/private" ;; destination
;;;                       ,(local-file (dotfiles-home "/.emacs.d/private")
;;;                                    #:recursive? #t)))
;;; because:
;;; 1. Can't store the whole ".emacs.d/private" since there are some README.md
;;; files and `git ... rebase develop cycle' b/c they will be symlinked (from
;;; the /gnu/store/).
;;;
;;; 2. Can't store the ".emacs.d/private" w/o the README.md files and restore
;;; them after `guix home ...', since `git restore ...' overwrites the symlink
;;; (to the /gnu/store/).
                       (list
                        (let ((dir "bin"))
                          `(,dir ;; destination
                            ,(local-file (dotfiles-home "/" dir)
                                         #:recursive? #t)))
                        (let ((destination
                               (str ".emacs.d"
                                    "/private/themes"
                                    "/farmhouse-light-mod-theme"))
                              (dir (str ".emacs.d/private/local"
                                        "/farmhouse-light-mod-theme")))
                          `(,destination ,(local-file (dotfiles-home "/" dir)
                                                      #:recursive? #t)))
;;; See value of `spacemacs-data-directory' in the $dev/guix-packages/spacemacs
                        (let ((destination
                               (str ".local/share/spacemacs"
                                    "/private/themes"
                                    "/farmhouse-light-mod-theme"))
                              (dir (str ".emacs.d/private/local"
                                        "/farmhouse-light-mod-theme")))
                          `(,destination ,(local-file (dotfiles-home "/" dir)
                                                      #:recursive? #t)))))
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
              (partial append-fish-config-dir "/completions")
              (partial append-fish-config-dir "/conf.d")
              (partial append-fish-config-dir "/functions")
              (partial remove unspecified?)
              (partial append
                       (map
                        (lambda (filepath)
                          (if (equal? filepath "/fish_variables")
                              (let* [(src (fish-config-dotfiles filepath))
;;; TODO use 'user-home' instead of 'str home ...'
                                     (dst (str home "/"
                                               (fish-config-base filepath)))]
;;; TODO is this sexp is not executed because of lazy-evaluation?
                                (begin
                                  (format #t "(copy-file ~a ~a) ... " src dst)
                                  (let ((retval (copy-file src dst)))
                                    (format #t "retval: ~a\n" retval)
                                    ;; retval is #<unspecified>
                                    retval)))
                              `(,(fish-config-base filepath)
                                ,(local-file
                                  (fish-config-dotfiles filepath)))))
                        (list
                         #;"/fish_variables"
                         "/fish_plugins"))))))))
;;; TODO The (copy-file ...) is not an atomic operation, i.e. it's not undone
;;; when the 'guix home reconfigure ...' fails or is interrupted.
      (let* [(filepath "/fish_variables")
             (src (fish-config-dotfiles filepath))
             (dst (str home "/" (fish-config-base filepath)))]
        (let [(indent (str indent indent-inc))]
          (format #t "~a(copy-file ~a ~a) ... " indent src dst)
          (let ((retval (copy-file src dst)))
            (format #t "retval: ~a\n" retval)
            retval))

        #|
;;; Just changing ownership and permissions of `fish_variables' doesn't work:
        (begin
        ;; .rw-r--r-- bost users fish_variables
        (format #t "(chown ~a ~a ~a)\n" dst (getuid) (getgid))
        (chown dst (getuid) (getgid))
        ;; .rw-r--r-- fish_variables
        (format #t "(chmod ~a ~a)\n" dst #o644)
        (chmod dst #o644))
        |#)
      (format #t "Running ~a ... done\n" srvc-name)
      simple-srvc)))

(define scheme-files-service
  (let [(srvc-name 'scheme-files-service)]
    (format #t "Running ~a ... \n" srvc-name)
    (let [(simple-srvc
           (simple-service
            srvc-name home-files-service-type
            (list
;;; TODO `gui' should do `cd ~/dev/guix'
;;; TODO `guixg' should do `git pull --rebase' (preferably from a local guix
;;; checkout)
;;; TODO crc should search in the $dec
             (search-notes #:program-name "crc"  #:files "clojure")
;;; TODO cre should also search in the ~/.emacs.d/, ~/.spacemacs, kill-buffes
;;; and my=tweaks, farmhouse-light-mod
             (search-notes #:program-name "cre"  #:files "vim|emacs|org_mode")
             (search-notes #:program-name "crep" #:files ".*")
             (search-notes #:program-name "crf"  #:files "find_and_grep")
;;; TODO crg should also search in the $dotf/guix/
             (search-notes #:program-name "crg"  #:files "guix|guile")
;;; TODO crgi should also search in the output of `git config --get',
;;; ~/.gitconfig, etc.
             (search-notes #:program-name "crgi" #:files "git")
;;; TODO crl should search in the $dotf/.config/fish .bashrc, .bash_profile (and
;;; other profile files), etc.
             (search-notes #:program-name "crl"
                           #:files "guix|shells|linux|android")
;;; TODO crr should also search in the $der
             (search-notes #:program-name "crr"  #:files "racket")
;;; TODO crs should be like crl
             (search-notes #:program-name "crs"  #:files "shells")
             (search-notes #:program-name "cru"  #:files "utf8")
             (chmod-plus   #:program-name "prw"  #:chmod-params "rw")
             (chmod-plus   #:program-name "px"   #:chmod-params "x")
             (service-file #:program-name "c"       #:desc "batcat"
                           #:scheme-file-name "bat")
             (service-file #:program-name "e"       #:desc "emacs-launcher"
                           #:scheme-file-name "emacs-launcher")
             (service-file #:program-name "s"       #:desc "spguimacs-launcher"
                           #:scheme-file-name "spguimacs-launcher")
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
                           #:desc "list-directory-contents-with-full-paths"
                           #:scheme-file-name "lf")
             (service-file #:program-name "qemu-vm" #:desc "qemu-virt-machine")
             (service-file #:program-name "spag"
                           #:desc "spacemacs-git-fetch-rebase"))))]
      (format #t "Running ~a ... done\n" srvc-name)
      simple-srvc)))

;; Note: `home-environment' is (lazily?) evaluated as a last command
;; (let ((he (home-environment ...))) (format #t "Should be last\n") he)
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
  (map (compose identity list
;;; TODO what's the difference between specification->package+output and
;;; specification->package ?
                specification->package+output)
       (append
;;; activate the following sexp one by one when on a slow computer or
;;; connectivity
        (basic-profile-packages)
        (user-profile-packages)
        (kde-dependent-packages)
        (slow-packages)
        (packages-from-additional-channels)
        (spguimacs-packages)
        )))

;;; TODO see [PATCH] services: Add udev-rules-service helper.
;;; https://issues.guix.gnu.org/40454

 (services
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
         ;; (local-file ".bashrc" (fix-leading-dot ".bashrc"))
         (local-file
          (dotfiles-home "/guix/home/" filename)
          (fix-leading-dot filename)))))

     ;; List of file-like objects, which will be ADDED(!) to .bash_profile
     (bash-profile
      (list
       (bash-config-file
        "bash-profile"
        (str
         "\n" "export HISTFILE=" (getenv "XDG_CACHE_HOME")
         "/.bash_history"))
       ;; (local-file ".bashrc" "bash_profile") should work too
       ;; (local-file
       ;;  (dotfiles-home "/.bash_profile_additions")
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
   (service
    home-fish-service-type
    ;; fish configuration - see ~/dev/guix/gnu/home/services/shells.scm
    (home-fish-configuration
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
        (fish-config-dotfiles "/config.fish"))))))

   environment-variables-service
   home-dir-config-service
   scheme-files-service

   #;mcron-service

   ;; https://github.com/babariviere/dotfiles/blob/1deae9e15250c86cc235bb7b6e69ea770af7b13a/baba/home/gaia.scm
   ;; https://github.com/babariviere/dotfiles/blob/guix/baba/home/gaia.scm
   ;;; [WIP] home: Add home-git-service-type https://issues.guix.gnu.org/54293
   ;;; is not pulled yed
   ;; (service home-git-service-type
   ;;          (home-git-configuration
   ;;           (config
   ;;            `((user
   ;;               ((name . ,user-full-name)
   ;;                (email . ,user-mail-address)
   ;;                #;(signingKey . "...")))
   ;;              (github
   ;;               ((user . "Bost")))
   ;;              (remote
   ;;               ((pushDefault . "origin")))
   ;;              #;(commit ((gpgSign . #t)))
   ;;              #;(tag ((gpgSign . #t)))))))
   )))
