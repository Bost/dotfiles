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
guix home --allow-downgrades -L $dxh reconfigure $dxh/home-config-lukas.scm
# -L --load-path

# The tilda `~' is only expanded by shells when it's the first character of a
# command-line argument. Use $HOME instead

guix shell --development guix help2man git strace --pure
./pre-inst-env guix repl

;; see 'include', which unlike 'load', also works within nested lexical contexts
;; can't use the `~'
(load "/home/bost/dev/dotfiles.dev/guix/home/home-config-lukas.scm")
|#
;; (format #t "[home-config-lukas] evaluating module ...\n")

(define-module (home-config-lukas)
  #:use-module (utils)
  #:use-module (memo)

  ;; the code of this module comes in via the 'bost' channel
  ;; #:use-module (bost utils)
  #:use-module ((fs-utils) #:prefix hf:)

  ;; #:use-module ((cfg packages all-new) #:prefix hp:)
  #:use-module ((cfg packages all) #:prefix hp:)
  ;; #:use-module (cfg mcron)
  #:use-module (srvc fish)
  #:use-module (srvc dirs)
  #:use-module (srvc scheme-files)
  #:use-module ((home-config-base) #:prefix base:)
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

;; TODO consider putting home-config-ecke and system-configuration in one file
;; (if (getenv "RUNNING_GUIX_HOME") home system)

(when (is-system-lukas)
  (let* []
    (define m (module-name-for-logging))
    ;; (format #t "~a evaluating module ...\n" m)

    (define indent "")
    (define indent-inc "   ")

    (define (environment-vars list-separator)
      ((compose
        (partial
         append
         `(
           ("cores" . "2") ;; for --cores=$cores; see `jobs=$[$(nproc) * 95 / 100]'
           ;; TODO test if the library exists:
           ;;   test -e $LDP && set --export LD_PRELOAD $LDP
           ;; ("LD_PRELOAD" . "/usr/lib/x86_64-linux-gnu/libgtk3-nocsd.so.0")
          )))
       (base:environment-vars list-separator)))
    (testsymb 'environment-vars)

    ;; Note: `home-environment' is (lazily?) evaluated as a last command
    ;; (let ((he (home-environment ...))) (format #t "Should be last\n") he)
    (define home-env
      (home-environment
       (packages (hp:packages-to-install))
       (services
        ((compose
          ;; (lambda (v) (format #t "~a 0\n" m) v)
          (partial append (base:environment-variables-service
                           (environment-vars hf:list-separator-bash))))
         base:services))))
    (testsymb 'home-env)

    ;; (format #t "~a module evaluated\n" m)
    home-env))
