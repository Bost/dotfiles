#|
## Initially:
## 1. From another host:
# By default git is not installed in the raw Guix iso image / installation.
# Clone the repo to /tmp so that no uncommitted files (with secrets) are transferred
git clone $dotf /tmp/dotf && scp -r /tmp/dotf edge:/tmp

## 2. From the edge machine:
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

(define-module (syst-edge)
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
(use-service-modules
 cups desktop networking ssh
 vnc      ; for xvnc-service-type
 lightdm  ; for lightdm-service-type
 xorg     ; for gdm-service-type
 )

;; no need to write: #:use-module (gnu packages <module>)
(use-package-modules
 android  ; android-udev-rules - access smartphone via mtp://
 bash
 libusb   ; libmtp
 shells   ; login shell
 )

(evaluating-module)

(define keyb-layout
  (keyboard-layout
   "us,de,sk,fr" "altgr-intl,,qwerty,"
   #:options '("compose:menu,grp:ctrls_toggle")))

(define xorg-conf
  (xorg-configuration (keyboard-layout keyb-layout)))

(define lightdm-conf
  (lightdm-configuration
   (xorg-configuration xorg-conf)
   ;; (allow-empty-passwords? #t)

   ;; XDMCP is not encrypted; it has more modern
   ;; alternatives VNC, RDP
   ;; (xdmcp? #t)

   ;; should be the TigerVNC for remote desktop access
   (vnc-server? #t)

   ;; (vnc-server-command
   ;;  (file-append tigervnc-server "/bin/Xvnc"
   ;;               "  -SecurityTypes None"))
   ;; (seats
   ;;  (list (lightdm-seat-configuration
   ;;         (name "*")
   ;;         (user-session "ratpoison"))))
   ))

(define lightdm-srvc
  (service lightdm-service-type lightdm-conf))

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

    (host-name host-edge)
    (users (base:users-config (list
                               "video"  ;; video devices, e.g. webcams
                               "lp"     ;; control bluetooth devices
                               )))

    (locale "fr_FR.utf8")

    ;; Packages installed system-wide.  Users can also install packages
    ;; under their own account: use 'guix search KEYWORD' to search
    ;; for packages and 'guix install PACKAGE' to install a package.
    (packages
     (append
      (map specification->package
           (list
            "brightnessctl" #| backlight and LED brightness control |#
            ))
      packages-to-install
      %base-packages))

    ;; System services. Search for available services by
    ;;     guix system search KEYWORD

    (services
     (append
      base:services
      (list
       ;; TODO lightdm doesn't work properly. The login fails
       ;; lightdm-srvc
       (service xvnc-service-type (xvnc-configuration
                                   (display-number 5)
                                   (localhost? #f)
                                   (geometry
                                    "1920x1080"
                                    ;; "2880x1620"
                                    ;; "2880x1800"
                                    ;; "2880x1800*"
                                    )
                                   #;(xdmcp? #t)
                                   #;(inetd? #t)))
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
                             %default-authorized-guix-keys))))
        (gdm-service-type config => (gdm-configuration
                                     (inherit config)
                                     (auto-suspend? #f)
;;; See the Warning above in the xvnc-configuration
                                     #;(xdmcp? #t)
                                     ))
        #;(delete gdm-service-type))))

    (bootloader
     (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets (list "/boot/efi"))
      ;; keyboard-layout for the GRUB
      (keyboard-layout keyboard-layout)))

    ;; The list of file systems that get "mounted".  The unique
    ;; file system identifiers there ("UUIDs") can be obtained
    ;; by running 'blkid' in a terminal.
    (file-systems
     (cons* (file-system
              (mount-point "/boot/efi")
              (device (uuid "2A99-FCA1" 'fat32))
              (type "vfat"))
            (file-system
              (mount-point "/")
              (device (uuid "61048634-de01-4e76-ae08-4e1ae09b63f5" 'ext4))
              (type "ext4"))
            %base-file-systems))

    (swap-devices (list
                   (swap-space
                    (target (uuid "875b3ddd-1d5a-4358-9285-b6fbf4007d15")))))))

(module-evaluated)

;; operating-system (or image) must be returned
syst-config
