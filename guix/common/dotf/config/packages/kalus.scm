;;; Packages of the kalus machine, shared between:
;;;     guix system reconfigure guix/systems/syst-kalus.scm
;;; and the guix shell container emulating kalus:
;;;     guix/systems/guix-shell-kalus.scm
;;;
;;; The (config packages *) modules aren't under guix/common, so they're
;;; referred to via `@' and resolved only when called:
;;; - `kalus-system-packages' needs (config packages syst-all), i.e.
;;;     --load-path=/home/bost/dev/dotfiles/guix/systems/common
;;;   `-L guix/common' alone makes Guix scan this module for packages, e.g. in
;;;   `guix pull'; a #:use-module would then fail with
;;;     no code for module (config packages syst-all)
;;; - `kalus-home-packages' needs (config packages home-all), i.e.
;;;     --load-path=/home/bost/dev/dotfiles/guix/home/common
;;;   `guix system reconfigure' loads only guix/common and guix/systems/common.

(define-module (dotf config packages kalus)
  #:use-module (bost common utils)
  #:use-module (gnu)                       ; %base-packages, use-package-modules
  #:use-module (guix packages)             ; package?, package-name
  #:use-module (guix profiles)             ; packages->manifest
  #:use-module (srfi srfi-1)               ; remove
  )

(use-package-modules
 gnupg           ;; pinentry
 )

(define m (module-name-for-logging))
(evaluating-module)

(def (kalus-excluded-package-names)
  "Names of packages from `syst-packages-to-install' that kalus doesn't get,
unlike the other machines sharing it. kalus is offline; openssh was probably
needed only for its setup."
  '("openssh"))
(testsymb 'kalus-excluded-package-names)

(def-public (kalus-system-packages)
  "Packages installed system-wide on kalus."
  (append
   (list
    ;; Provides a console that allows users to enter a passphrase when
    ;; `gpg' is run and needs it.
    pinentry)      ; Seems like just 'pinentry-tty' doesn't do the job
   (remove (lambda (p)
             (and (package? p)
                  (member (package-name p) (kalus-excluded-package-names))))
           ((@(config packages syst-all) syst-packages-to-install)))
   %base-packages))
(testsymb 'kalus-system-packages)

(def-public (kalus-home-packages)
  "Packages of the kalus home profile, i.e. what `home-packages-to-install'
returns when the hostname is kalus."
  ((@(config packages home-all) basic-packages)))
(testsymb 'kalus-home-packages)

(def-public (kalus-manifest)
  "Manifest of all the system-wide and home packages of kalus."
  (packages->manifest
   (append (kalus-system-packages) (kalus-home-packages))))
(testsymb 'kalus-manifest)

(module-evaluated)
