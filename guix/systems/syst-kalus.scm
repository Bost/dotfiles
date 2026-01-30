(define-module (syst-kalus)
  #:use-module ((syst-base) #:prefix syst-base:)
  #:use-module (dotf settings)
  #:use-module (dotf utils)                 ; partial, module-name-for-logging
  #:use-module (dotf memo)
  #:use-module (gnu)
  #:use-module (guix modules)
  #:use-module (guix)                  ; package-version
  )

(use-service-modules ; no need to write: #:use-module (gnu services <module>)
 networking      ; network-manager-service-type, wpa-supplicant-service-type
 ssh             ; openssh-service-type
 )

(use-package-modules ; no need to write: #:use-module (gnu packages <module>)
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

(define-public syst-config
  (operating-system
    (inherit (syst-base:syst-config))
    (keyboard-layout
     (keyboard-layout "us" "altgr-intl"))
    (host-name host-kalus)
    (users (syst-base:users-config))

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
      openssh-sans-x
      rsync           ;; 'scp' is preinstalled
      strace
      vim             ;; 'vi' is preinstalled
      wget
      %base-packages))

    (services
     (cons*
      (service network-manager-service-type)
      ;; Prevent error ... 'NetworkManager' requires 'wireless-daemon' ...
      (service wpa-supplicant-service-type)
      (service
       openssh-service-type
       (openssh-configuration
         (openssh openssh-sans-x)
         (password-authentication? #false)
         (authorized-keys
          ;; Assuming the local-file exists under given path, e.g. it was
          ;; transferred by `ssh-copy-id` at some point in the past.
          `((,user
             ,(local-file (string-append home "/.ssh/authorized_keys")))))))
      %base-services))

;;; See
;;; https://guix.gnu.org/manual/en/html_node/Bootloader-Configuration.html
;;; https://www.gnu.org/software/grub/manual/grub/html_node/Invoking-grub_002dinstall.html#Invoking-grub_002dinstall
;;; https://github.com/babariviere/dotfiles/blob/guix/baba/bootloader/grub.scm
    (bootloader
      (bootloader-configuration
        (bootloader grub-efi-bootloader)
        (targets (list "/boot/efi"))
        ;; keyboard-layout for the GRUB
        (keyboard-layout keyboard-layout)))

;;; The list of file systems that get "mounted". The unique file system
;;; identifiers there ("UUIDs") can be obtained by running 'blkid' in a
;;; terminal.
    (file-systems
     (cons*
      (file-system
        (mount-point "/boot/efi")
        (device (uuid "C433-B4CD" 'fat32))
        (type "vfat"))
      (file-system
        (mount-point "/")
        (device (uuid "16e81bde-2975-4814-aef9-9d06484dd5e2" 'ext4))
        (type "ext4"))
      %base-file-systems))
    (swap-devices (list
                   (swap-space
                     (target (uuid "566ddff0-d6ee-4d7e-8cc9-ec273120617e")))))))

(module-evaluated)

syst-config ;; operating-system (or image) must be returned
