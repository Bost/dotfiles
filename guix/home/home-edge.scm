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
     -L $dx/common -L $dx/home/common reconfigure $dx/home/home-$(hostname).scm
# -L --load-path

# The tilda `~' is only expanded by shells when it's the first character of a
# command-line argument. Use $HOME instead.

;; see 'include', which unlike 'load', works within nested lexical contexts
;; can't use the `~'
(load "/home/bost/dev/dotfiles/guix/home/home-ecke.scm")

|#

;; The 'edge' and 'ecke' home environments are almost the same, and it may be
;; enough to handle the differences just a few branching statements, e.g.
;; if, cond, etc.
(define-module (home-edge)
  #:use-module (utils)
  #:use-module (settings)
  #:use-module (memo)
  #:use-module (fs-utils)
  #:use-module (srvc fish)
  #:use-module (srvc dirs)
  #:use-module (srvc scheme-files)
  #:use-module ((home-base) #:prefix base:)
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

(base:install-all-projects)

;;     (begin
;;       ;; fish-config-base and fish-config-dotfiles are also defined in the (srvc fish)
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

;;; TODO see also the xfce4 chromium launcher -> command
;;; /home/bost/.guix-profile/bin/chromium %U

;;; TODO see [PATCH] services: Add udev-rules-service helper.
;;; https://issues.guix.gnu.org/40454

;;; TODO home-git-configuration

(define home-env (base:home-env-edge-ecke list-separator-bash))
(testsymb 'home-env)

(module-evaluated)
home-env
