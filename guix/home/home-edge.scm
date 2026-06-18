;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

#|
# To prevent incorrect values in the ~/.guix-home/setup-environment (e.g.
# XDG_DATA_DIRS), reset environment variables to their default values by
# sourcing the default bash profile and run `guix home ...` command from bash:

source /etc/profile && baseLP=$HOME/dev/dotfiles/guix
guix home --allow-downgrades \
     -L $baseLP/common -L $baseLP/home/common \
     reconfigure $baseLP/home/home-$(hostname).scm
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
  #:use-module (dotf utils)
  #:use-module (dotf settings)
  #:use-module (dotf memo)
  #:use-module (dotf fs-utils)
  #:use-module (services development-dirs)
  #:use-module (services blueman-applet-autostart)
  #:use-module (services cli-utils)
  #:use-module ((home-base) #:prefix home-base:)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services)
  #:use-module (srfi srfi-1) ; list-processing procedures

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

(home-base:install-all-projects-edge-ecke)

;;; TODO see also the xfce4 chromium launcher -> command
;;; /home/bost/.guix-profile/bin/chromium %U

;;; TODO see [PATCH] services: Add udev-rules-service helper.
;;; https://issues.guix.gnu.org/40454

;;; TODO home-git-configuration

(def home-env
     (home-environment
      ;; (packages ...) replaced by $dotf/guix/profile-manifest.scm
      ;; (packages (home-packages-to-install))
      (services
       (append
        (blueman-services)
        ;; (list (blueman-applet-autostart-service))
        (home-base:home-env-services-edge-ecke)))))

(module-evaluated)
home-env
