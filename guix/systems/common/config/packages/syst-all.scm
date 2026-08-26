(define-module (config packages syst-all)
  #:use-module (bost common utils)
  #:use-module (gnu)   ; use-package-modules
  #:use-module (dotf config channels channel-defs) ; channel-guix
  #:use-module (dotf config packages package-defs)  ; pkg-or-inferior
  )

(use-package-modules
 base            ;; glibc
 cups            ;; cups
 disk            ;; gparted
 gnupg           ;; gpg, pinentry
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

(def-public (syst-packages-to-install)
  ;; (format #t "~a Starting…\n" f)
  ((comp
    ;; (lambda (p) (format #t "~a done\n" f) p)
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

    (pkg-or-inferior
     gnupg
     #:channels
     (list (channel-guix
            #:commit
            ;; Last working guix pull from 20 aug 2026 22:39:36
            "c98ec501cce5c4776602ae7cb90b0ba5962ee895"

            ;; Causes https://codeberg.org/guix/guix/issues/10622
            ;; CommitDate: Mon Aug 17 11:00:35 2026 +0200
            ;; gnu: gnupg: Update to 2.5.20.
            ;; "e660026a1625db74f844bbd96a6681ca0fe922b3"

            ;; Last before the "gnu: gnupg: Update to 2.5.20."
            ;; Can't use it. Causes too may builds
            ;; "1b19586c07b586d6fb8b3ead00153fbeb882faf8"
            )))
    gparted    ;; disk partition
    mtools     ;; used by gparted
    rsync      ;; 'scp' is preinstalled
    vim        ;; 'vi' is preinstalled
    )))
(testsymb-trace 'syst-packages-to-install)

(module-evaluated)
