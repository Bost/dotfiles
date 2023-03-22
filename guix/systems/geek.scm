#|
## Initially:
## 1. From another host:
scp ~/.config/guix/channels.scm geek:~/.config/guix/
cd $dev/dotfiles/guix/systems && scp signing-key.pub geek.scm geek:/tmp

## 2. From the geek machine:
chmod +rw ~/.config/guix/channels.scm
vi ~/.config/guix/channels.scm # remove all other channels except 'nonguix'
guix pull
sudo guix system --fallback reconfigure geek.scm

## Run this file by (the `~' doesn't work as a value of --load-path):
# --fallback         fall back to building when the substituter fails
# -L --load-path
sudo guix system --fallback -L $dotf/guix/systems reconfigure $dotf/guix/systems/geek.scm
|#

;; (format #t "[geek] evaluating module ...\n")

(define-module (geek)
  #:use-module (gnu)
  #:use-module (gnu system shadow)     ; for user-group; user-account-shell
  #:use-module (common settings)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  )

(use-service-modules
 cups
 desktop
 networking
 ssh
 xorg)

(define operating-system-configuration
  (operating-system
    (kernel linux)
    (initrd microcode-initrd)
    (firmware (list linux-firmware))
    (locale "en_US.utf8")
    (timezone "Europe/Berlin")
    (keyboard-layout
     (keyboard-layout
      "us,de,sk" "altgr-intl,,qwerty"
      #:options '("compose:menu,grp:ctrls_toggle")))
    (host-name "geek")

    ;; The list of user accounts ('root' is implicit).
    (users (cons* (user-account
                   (name "bost")
                   (comment "Rostislav Svoboda")
                   (group "users")
                   (home-directory "/home/bost")
                   (supplementary-groups
                    '("wheel" ;; gives access to 'sudo'
                      "netdev" "audio" "video")))
                  %base-user-accounts))

;;; Packages installed system-wide. Users can also install packages under their
;;; own account: use 'guix search KEYWORD' to search for packages and 'guix
;;; install PACKAGE' to install a package.
    (packages
     (append
      (map specification->package
           (list
            "brightnessctl" ; backlight and LED brightness control
            "git"
            ;; "gparted"    ; disk partition
            "nss-certs"
            ;; "rsync"      ; 'scp' is preinstalled
            ;; "vim"        ; 'vi' is preinstalled
            ))
      %base-packages))

;;; Below is the list of system services. To search for available services, run
;;; 'guix system search KEYWORD' in a terminal.
    (services
     (append (list
              (service xfce-desktop-service-type)

              ;; for the NetworkManager
              (service network-manager-service-type)

              (service wpa-supplicant-service-type)

              ;; ntp-service-type for system clock sync is in the
              ;; %desktop-services by default

              ;; To configure OpenSSH, pass an 'openssh-configuration'
              ;; record as a second argument to 'service' below.
              (service openssh-service-type)

              (set-xorg-configuration
               (xorg-configuration (keyboard-layout keyboard-layout))))

             ;; This is the default list of services we are appending to.
             (modify-services %desktop-services
               (guix-service-type
                config => (guix-configuration
                           (inherit config)
                           (substitute-urls
                            (append (list "https://substitutes.nonguix.org")
                                    %default-substitute-urls))
                           (authorized-keys
;;; The signing-key.pub should be obtained by
;;;   wget https://substitutes.nonguix.org/signing-key.pub
                            (append (list (local-file "./signing-key.pub"))
                                    %default-authorized-guix-keys)))))))
    (bootloader
     (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets (list "/boot/efi"))
      (keyboard-layout keyboard-layout)))
    (mapped-devices (list (mapped-device
                           (source (uuid
                                    "fe8beac5-240d-4850-b67c-347b8cf4dc7e"))
                           (target "encrypted")
                           (type luks-device-mapping))))

    ;; The list of file systems that get "mounted".  The unique
    ;; file system identifiers there ("UUIDs") can be obtained
    ;; by running 'blkid' in a terminal.
    (file-systems (cons* (file-system
                           (mount-point "/")
                           (device "/dev/mapper/encrypted")
                           (type "ext4")
                           (dependencies mapped-devices))
                         (file-system
                           (mount-point "/boot/efi")
                           (device (uuid "FC73-7111"
                                         'fat32))
                           (type "vfat")) %base-file-systems))))

;; (format #t "[geek] module evaluated\n")

;; operating-system (or image) must be returned
operating-system-configuration
