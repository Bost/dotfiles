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

(evaluating-module)

(define-public syst-config
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
             (name user)
             (comment
              (begin
                ;; (format #t "~a user-full-name: ~a\n" m user-full-name)
                user-full-name))
             (group "users")
             (home-directory home)

             ;; list of group names that this user-account belongs to
             (supplementary-groups
              ;; grant access to:
              '("wheel"  #| sudo etc.; See polkit-wheel-service for administrative tasks for non-root users |#
                "netdev" #| network devices |#
                "audio"  #| sound card |#
                "video"  #| video devices, e.g. webcams |#
                "lp"     #| control bluetooth devices |#

                ;; "kvm"
                ;; "tty"
                ;; "input"
                ;; "docker"
                ;; "realtime"  #| Enable realtime scheduling |#
                )))
            %base-user-accounts))

;;; Packages installed system-wide. Users can also install packages under their
;;; own account: use 'guix search KEYWORD' to search for packages and 'guix
;;; install PACKAGE' to install a package.
    (packages
     (append
      (map specification->package
           (list
            "brightnessctl" #| backlight and LED brightness control |#
            ))
      packages-to-install
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

(module-evaluated)

;; operating-system (or image) must be returned
syst-config
