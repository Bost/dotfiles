(define-module (syst-lukas)
  #:use-module ((syst-base) #:prefix base:)
  #:use-module (settings)
  #:use-module (utils)                 ; partial, module-name-for-logging
  #:use-module (memo)
  #:use-module (gnu)
  #:use-module (guix)                  ; package-version
)

;; no need to write: #:use-module (gnu services <module>)
(use-service-modules
 desktop
 networking      ;; dhcp-client-service-type
 ssh             ;; openssh-service-type
 )

(use-package-modules
 gnupg           ;; gpg
 linux           ;; iptables (IP packet filtering rules)
 rsync
 ssh             ;; openssh
 version-control ;; git
 vim
 wget            ;; wget

 admin
 package-management
 tls
 )

(define m (module-name-for-logging))
(evaluating-module)

;; (define minimal-desktop-services
;;   (list polkit-wheel-service
;;         (service upower-service-type)
;;         (service accountsservice-service-type)
;;         (service polkit-service-type)
;;         (service elogind-service-type)
;;         (service dbus-root-service-type)
;;         (service x11-socket-directory-service-type)))

(define-public syst-config
  (operating-system
    (inherit (base:syst-config))
    (keyboard-layout
     (keyboard-layout "us" "altgr-intl"))
    (host-name host-lukas)
    (users (base:users-config (list
                               "cdrom" ;; access to CD-ROM
                               )))

;;; Packages installed system-wide. Users can also install packages under their
;;; own account: use 'guix search KEYWORD' to search for packages and 'guix
;;; install PACKAGE' to install a package.
;;;
;;; Install git & rsync system-wide to be able to git-clone / rsync the dotfiles
    (packages
     (cons*

      ;; From the comment in gnu/packages/version-control.scm
      ;; The size of the closure of 'git-minimal' is two thirds that of 'git'.
      ;; Its test suite runs slightly faster and most importantly it doesn't
      ;; depend on packages that are expensive to build such as Subversion.
      git-minimal     ;; git

      gnupg
      iptables        ;; Programs to configure Linux IP packet filtering rules
      openssh
      openssh-sans-x
      rsync           ;; 'scp' is preinstalled
      strace
      vim             ;; 'vi' is preinstalled
      wget
      %base-packages))

    (services
     (cons*
      (service openssh-service-type
               #;
               (openssh-configuration
                (openssh openssh-sans-x)
                (password-authentication? #false)
                (authorized-keys
                 `(("janedoe" ,(local-file "janedoe_rsa.pub"))
                   ("root" ,(local-file "janedoe_rsa.pub"))))))
      %desktop-services))

;;; See
;;; https://guix.gnu.org/manual/en/html_node/Bootloader-Configuration.html
;;; https://www.gnu.org/software/grub/manual/grub/html_node/Invoking-grub_002dinstall.html#Invoking-grub_002dinstall
;;; https://github.com/babariviere/dotfiles/blob/guix/baba/bootloader/grub.scm
    (bootloader
     (bootloader-configuration
      (bootloader grub-bootloader)
      (targets (list "/dev/sda"))
      ;; keyboard-layout for the GRUB
      (keyboard-layout keyboard-layout)))

;;; The list of file systems that get "mounted". The unique file system
;;; identifiers there ("UUIDs") can be obtained by running 'blkid' in a
;;; terminal.
    (file-systems
     (cons*
      (file-system
        (mount-point "/")
        (device (uuid "cf11628d-4887-42d2-aef1-635ad5089ce1" 'ext4))
        (type "ext4"))
      %base-file-systems))
    (swap-devices (list
                   (swap-space
                    (target (uuid "a4767437-a9c8-4d57-9755-4fcd2aef73da")))))))

(module-evaluated)

syst-config ;; operating-system (or image) must be returned
