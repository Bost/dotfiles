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

(define-module (geek)
  #:use-module (settings)
  #:use-module (utils)                 ; for partial
  #:use-module (memo)

  #:use-module (gnu)
  #:use-module (gnu system shadow)     ; for user-group; user-account-shell
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (guix)                  ; for package-version
  )

;; no need to write: #:use-module (gnu services <module>)
(use-service-modules cups desktop networking ssh xorg)

;; no need to write: #:use-module (gnu packages <module>)
(use-package-modules
 android  ; android-udev-rules - access smartphone via mtp://
 bash
 libusb   ; libmtp
 shells   ; login shell
 )

(when (is-system-geek)
  (let* []
    (define m (module-name-for-logging))
    ;; (format #t "~a evaluating module ...\n" m)

    (define operating-system-configuration
      (operating-system
        (kernel linux)
        (initrd microcode-initrd)
        (firmware (list linux-firmware))
        (locale "en_US.utf8")
        (timezone "Europe/Berlin")
        (keyboard-layout ; keyboard-layout for the console
         (keyboard-layout
          "us,de,sk,fr" "altgr-intl,,qwerty,"
          #:options '("compose:menu,grp:ctrls_toggle")))
        (host-name host-geek)

        ;; The list of user accounts ('root' is implicit).
        (users (cons*
                (user-account
                 (name "bost")
                 (comment
                  (begin
                    ;; (format #t "~a user-full-name: ~a\n" m user-full-name)
                    user-full-name))
                 (group "users")
                 (home-directory "/home/bost")
                 (supplementary-groups
                  '("wheel" ;; gives access to 'sudo'
                    "netdev" "audio" "video"
                    "adbusers" ;; for android
                    )))
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

                  ;; ntp-service-type for system clock sync is in the
                  ;; %desktop-services by default

                  ;; To configure OpenSSH, pass an 'openssh-configuration'
                  ;; record as a second argument to 'service' below.
                  (service openssh-service-type)

                  (set-xorg-configuration
                   (xorg-configuration (keyboard-layout keyboard-layout)))

                  (udev-rules-service 'mtp libmtp) ;; mtp - Media Transfer Protocol
                  (udev-rules-service 'android android-udev-rules
                                      #:groups '("adbusers")))

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

    ((compose
      (partial format #t "~a kernel-version: ~a\n" m)
      package-version
      operating-system-kernel)
     operating-system-configuration)

    ;; (format #t "~a module evaluated\n" m)

    ;; operating-system (or image) must be returned
    operating-system-configuration))
