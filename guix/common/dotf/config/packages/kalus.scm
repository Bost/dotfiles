;;; Packages of the kalus machine, shared between:
;;;     guix system reconfigure guix/systems/syst-kalus.scm
;;; and the guix shell container emulating kalus:
;;;     /home/bost/dev/ai-coding-agents/guix-shell.new.scm
;;;
;;; `guix system reconfigure' loads only guix/common and guix/systems/common,
;;; so `kalus-home-packages' refers to (config packages home-all) via `@'. That
;;; module is resolved only when `kalus-home-packages' is called, which needs
;;;     --load-path=/home/bost/dev/dotfiles/guix/home/common

(define-module (dotf config packages kalus)
  #:use-module (bost common utils)
  #:use-module (gnu)                       ; %base-packages, use-package-modules
  #:use-module (guix profiles)             ; packages->manifest
  #:use-module (config packages syst-all)  ; syst-packages-to-install
  )

(use-package-modules
 gnupg           ;; pinentry
 )

(define m (module-name-for-logging))
(evaluating-module)

(def-public (kalus-system-packages)
  "Packages installed system-wide on kalus."
  (append
   (list
    ;; Provides a console that allows users to enter a passphrase when
    ;; `gpg' is run and needs it.
    pinentry)      ; Seems like just 'pinentry-tty' doesn't do the job
   (syst-packages-to-install)
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
