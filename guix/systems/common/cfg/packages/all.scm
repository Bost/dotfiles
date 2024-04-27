(define-module (cfg packages all)
  ;; provides: use-package-modules
  #:use-module (utils)
  #:use-module (gnu)
  )

(use-package-modules
 version-control ;; git
 gnupg           ;; gpg
 disk            ;; gparted
 mtools          ;; used by gparted
 rsync
 vim
 )

(define m (module-name-for-logging))
(evaluating-module)

(define-public (packages-to-install)
  (list
;;; Install git & rsync system-wide to be able to git-clone / rsync the dotfiles
   git
   gnupg
   gparted    ;; disk partition
   mtools     ;; used by gparted
   rsync      ;; 'scp' is preinstalled
   vim        ;; 'vi' is preinstalled
   ))
(testsymb-trace 'packages-to-install)

(module-evaluated)

