;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

#|
# To prevent incorrect values in the ~/.guix-home/setup-environment (e.g.
# XDG_DATA_DIRS), reset environment variables to their default values by
# sourcing the default bash profile and run `guix home ...` command from bash:
source /etc/profile && dx=$HOME/dev/dotfiles/guix
guix home --allow-downgrades --cores=$cores \
     -L $dx/common -L $dx/home reconfigure $dx/home/home-geek.scm
# -L --load-path

# The tilda `~' is only expanded by shells when it's the first character of a
# command-line argument. Use $HOME instead

;; see 'include', which unlike 'load', also works within nested lexical contexts
;; can't use the `~'
(load "/home/bost/dev/dotfiles.dev/guix/home/home-geek.scm")
|#

(define-module (home-geek)
  #:use-module (utils)
  #:use-module (settings)
  #:use-module (memo)
  #:use-module (scm-bin git-clone)
  #:use-module (fs-utils)
  #:use-module (config packages all)
  #:use-module (services fish)
  #:use-module (services dirs)
  #:use-module (services scheme-files)
  #:use-module ((home-base) #:prefix base:)
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
  ;; #:use-module (ice-9 pretty-print)

  ;; the https://issues.guix.gnu.org/51359 has not been merged yet
  ;; home-git-service-type
  ;; #:use-module (gnu home services version-control)
  )

(define m (module-name-for-logging))
(evaluating-module)

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

(format #t "~a obtaining projects…" m)
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
  (let* [(dest-dir-repo (str home dest-dir repo))
         (repo-url (if #f ; (url? repo)
                       repo
                       (str gitlab repo)))]
    (git-clone "--origin=gitlab" repo-url dest-dir-repo)
    (exec-system*
     "git" (str "--git-dir=" dest-dir-repo "/.git") "remote add github"
     (str github repo))))

;; Existing projects won't be overridden
;; (map (lambda (project)
;;        (let ((dest-dir (car project)))
;;          (map (partial obtain-and-setup dest-dir) (cdr project))))
;;      projects)
(format #t "done\n")

(format #t "~a obtaining projects-heroku… " m)
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
    (git-clone "--origin=vojto" repo-url dest-dir-repo)))

;; Existing projects won't be overridden
;; (map (lambda (project)
;;        (let ((dest-dir (car project)))
;;          (map (partial obtain-and-setup-heroku dest-dir) (cdr project))))
;;      projects-heroku)
(format #t "done\n")

;;     (begin
;;       ;; fish-config-base and fish-config-dotfiles are also defined in the (services fish)
;;       (define* (fish-config-base #:rest args)
;;         "(fish-config-base) ; => \".config/fish\""
;;         (apply str (basename xdg-config-home) "/fish" args))

;;       (define* (fish-config-dotfiles #:rest args)
;;         "(fish-config-dotfiles) ; => \"/home/bost/dev/dotfiles/.config/fish\"
;;     Note:
;;     (format #t \"~a\" \"foo\") doesn't work"
;;         (apply str (user-dotf) "/" (fish-config-base) args))

;; ;;; TODO The (copy-file ...) is not an atomic operation, i.e. it's not undone
;; ;;; when the 'guix home reconfigure ...' fails or is interrupted.
;; ;;; Can't use `local-file' or `mixed-text-file' or something similar since the
;; ;;; `fish_variables' must be editable
;;       (let* [(indent "")
;;              (indent-inc "   ")
;;              (filepath "/fish_variables")
;;              (src (fish-config-dotfiles filepath))
;;              (dst (user-home "/" (fish-config-base filepath)))
;;              (dstdir (dirname dst))]
;;         (unless (file-exists? dstdir)
;;           (let [(indent (str indent indent-inc))]
;;             (format #t "~a(mkdir ~a)… " indent src dstdir)
;;             (let ((retval (mkdir dstdir)))
;;               (format #t "retval: ~a\n" retval)
;;               ;; The value of 'retval' is '#<unspecified>'
;; ;;; TODO continuation: executing the block only if the dstdir was created.
;;               retval)))
;; ;;; TODO is this sexp is not executed because of lazy-evaluation?
;;         (let [(indent (str indent indent-inc))]
;;           (format #t "~a(copy-file ~a ~a)… " indent src dst)
;;           (let ((retval (copy-file src dst)))
;;             (format #t "retval: ~a\n" retval)
;;             ;; The value of 'retval' is '#<unspecified>'
;;             retval))
;; ;;; Just changing ownership and permissions of `fish_variables' doesn't work:

;;         ;; (begin
;;         ;;   ;; .rw-r--r-- bost users fish_variables
;;         ;;   (format #t "(chown ~a ~a ~a)\n" dst (getuid) (getgid))
;;         ;;   (chown dst (getuid) (getgid))
;;         ;;   ;; .rw-r--r-- fish_variables
;;         ;;   (format #t "(chmod ~a ~a)\n" dst #o644)
;;         ;;   (chmod dst #o644))
;;         ))

;; See also $dotf/.bashrc.martin
(define home-env
  (home-environment
   ;; Replaced by $dotf/guix/profile-manifest.scm
   ;; (packages ((@(config packages all) home-packages-to-install)))
   (services
    ((comp
      #;(lambda (v) (format #t "~a 3:\n~a\n" m v) v)
      (partial append (base:services))
      #;(lambda (v) (format #t "~a 2:\n~a\n" m v) v)
      list
      base:environment-variables-service
      #;(lambda (v) (format #t "~a 1:\n~a\n" m v) v)
      (partial
       append
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

         ;; open man-pages in nvim
         ("MANPAGER" . "nvim +Man!")

         ;; for `flatpak run ...`
         ("XDG_DATA_DIRS" . ,((comp
                               (lambda (lst) (string-join lst list-separator-bash)))
                              (list
                               (user-home "/.local/share/flatpak/exports/share")
                               "/var/lib/flatpak/exports/share"
                               (getenv "XDG_DATA_DIRS"))))

         ("dec"   . ,(user-home "/dec"))
         ("der"   . ,(user-home "/der"))
         ))
      #;(lambda (v) (format #t "~a 0:\n~a\n" m v) v))
     (base:environment-vars list-separator-bash)))))
(testsymb 'home-env)

(module-evaluated)
home-env
