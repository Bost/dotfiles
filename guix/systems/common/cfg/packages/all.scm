(define-module (cfg packages all)
  #:use-module (utils)
  #:use-module (gnu)   ; use-package-modules
  )

(use-package-modules
 base            ;; glibc
 cups            ;; cups
 disk            ;; gparted
 gnupg           ;; gpg
 linux           ;; iptables (IP packet filtering rules)
 mtools          ;; used by gparted
 rsync
 ssh             ;; openssh
 version-control ;; git
 vim
 wget            ;; wget
 xorg            ;; setxkbmap
 )

(define m (module-name-for-logging))
(evaluating-module)

;; (define my-glibc-locales
;;   (make-glibc-utf8-locales
;;    glibc
;;    #:locales (list "en_US" "de_DE" "sk_SK" "fr_FR")
;;    #:name "glibc-my-utf8-locales"))

(define-public (syst-packages-to-install)
  (let* [(m (format #f "~a [syst-packages-to-install]" m))]
    ;; (format #t "~a Starting…\n" m)
    ((comp
      ;; (lambda (p) (format #t "~a done.\n" m) p)
      )
     (list

      ;; Set the keyboard using the X Keyboard Extension
      ;; setxkbmap is probably not needed. Anyway,execute `loadkeys us` if
      ;; something goes wrong.
      setxkbmap

      ;; The Common Unix Printing System
      cups ;; provides lpinfo

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
      ))))
(testsymb-trace 'syst-packages-to-install)

(module-evaluated)
