(define-module (cfg packages all)
  ;; provides: use-package-modules
  #:use-module (utils)
  #:use-module (gnu)
  )

(use-package-modules
 disk            ;; gparted
 gnupg           ;; gpg
 linux           ;; iptables (IP packet filtering rules)
 mtools          ;; used by gparted
 rsync
 ssh             ;; openssh
 version-control ;; git
 vim
 wget            ;; wget
 )

(define m (module-name-for-logging))
(evaluating-module)

(define-public (packages-to-install)
  (list
;;; Install git & rsync system-wide to be able to git-clone / rsync the dotfiles
   ;; From the comment in gnu/packages/version-control.scm
   ;; The size of the closure of 'git-minimal' is two thirds that of 'git'.
   ;; Its test suite runs slightly faster and most importantly it doesn't
   ;; depend on packages that are expensive to build such as Subversion.
   git-minimal
   ;; git

   wget
   iptables   ;; Programs to configure Linux IP packet filtering rules
   openssh
   strace

   gnupg
   gparted    ;; disk partition
   mtools     ;; used by gparted
   rsync      ;; 'scp' is preinstalled
   vim        ;; 'vi' is preinstalled
   ))
(testsymb-trace 'packages-to-install)

(module-evaluated)

