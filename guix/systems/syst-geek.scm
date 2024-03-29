#|
## Initially:
## 1. From another host:
# By default git is not installed in the raw Guix iso image / installation.
# Clone the repo to /tmp so that no uncommitted files (with secrets) are transferred
git clone $dotf /tmp/dotf && scp -r /tmp/dotf geek:/tmp

## 2. From the geek machine:
dotf=/tmp/dotf
mkdir -p ~/.config/guix
cp $dotf/.config/guix/channels.scm ~/.config/guix
# Make sure the ~/.config/guix/channels.scm contains only 'nonguix' and %default-channels
guix pull
sudo guix system --fallback -L $dotf/guix/common -L $dotf/guix/systems/common reconfigure $dotf/guix/systems/syst-$(hostname).scm

## Run this file by (the `~' doesn't work as a value of --load-path):
# --fallback         fall back to building when the substituter fails
# -L --load-path
sudo guix system --fallback -L $dotf/guix/common -L $dotf/guix/systems/common reconfigure $dotf/guix/systems/syst-$(hostname).scm
|#

(define-module (syst-geek)
  #:use-module ((syst-base) #:prefix base:)
  #:use-module (settings)
  #:use-module (utils)                 ; for partial
  #:use-module (memo)
  #:use-module (cfg packages all)      ; for packages-to-install

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

(define m (module-name-for-logging))
(evaluating-module)

(define syst-config-linux
  (operating-system
    (inherit base:syst-config)
    (kernel linux)
    (initrd microcode-initrd)
    (firmware (list linux-firmware))))

(define-public syst-config
  (operating-system
    (inherit syst-config-linux)

    ;; keyboard-layout is not inherited
    (keyboard-layout base:keyb-layout)
    ;; (keyboard-layout (operating-system-keyboard-layout base:syst-config))

    (host-name host-geek)
    (users (base:users-config (list
                               "video"  ;; video devices, e.g. webcams
                               "lp"     ;; control bluetooth devices
                               )))

;;; Packages installed system-wide. Users can also install packages under their
;;; own account: use 'guix search KEYWORD' to search for packages and 'guix
;;; install PACKAGE' to install a package.
    (packages
     (append
      (map specification->package
           (list
            "brightnessctl" #| backlight and LED brightness control |#
            ))
      (packages-to-install)
      %base-packages))

;;; Below is the list of system services. To search for available services, run
;;; 'guix system search KEYWORD' in a terminal.
    (services
     (append
      (base:services)
      (list
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
                     ;; The signing-key.pub should be obtained by
                     ;; wget https://substitutes.nonguix.org/signing-key.pub
                     (append (list (local-file "./signing-key.pub"))
                             %default-authorized-guix-keys)))))))

    (bootloader
     (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets (list "/boot/efi"))
      ;; keyboard-layout for the GRUB
      (keyboard-layout keyboard-layout)))

    ;; The list of file systems that get "mounted".  The unique
    ;; file system identifiers there ("UUIDs") can be obtained
    ;; by running 'blkid' in a terminal.
    (file-systems (cons* (file-system
                           (mount-point "/boot/efi")
                           (device (uuid "FC73-7111" 'fat32))
                           (type "vfat"))
                         (file-system
                           (mount-point "/")
                           (device "/dev/mapper/encrypted")
                           (type "ext4")
                           (dependencies mapped-devices))
                         %base-file-systems))

    (mapped-devices (list
                     (mapped-device
                      (source (uuid "fe8beac5-240d-4850-b67c-347b8cf4dc7e"))
                      (target "encrypted")
                      (type luks-device-mapping))))))

(module-evaluated)

;; operating-system (or image) must be returned
syst-config
