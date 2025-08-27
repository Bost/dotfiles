;; ## Initially:
;; ## 1. From another host:
;; # By default git is not installed in the raw Guix iso image / installation.
;; # Clone the repo to /tmp so that no uncommitted files (with secrets) are transferred
;; git clone $dotf /tmp/dotf && scp -r /tmp/dotf geek:/tmp
;;
;; ## 2. From the geek machine:
;; dotf=/tmp/dotf
;; mkdir -p ~/.config/guix
;; cp $dotf/.config/guix/channels.scm ~/.config/guix
;;
;; ## Run this file by (the `~' doesn't work as a value of --load-path):
;; # --fallback         fall back to building when the substituter fails
;; # -L --load-path
;; sudo guix system --fallback -L $dotf/guix/common -L $dotf/guix/systems/common reconfigure $dotf/guix/systems/syst-$(hostname).scm

(define-module (syst-geek)
  #:use-module ((syst-base) #:prefix base:)
  #:use-module (settings)
  #:use-module (utils)                 ; partial
  #:use-module (memo)
  #:use-module (config packages all)   ; syst-packages-to-install
  #:use-module (gnu)
  #:use-module (gnu system shadow)     ; user-group; user-account-shell
  #:use-module (guix)                  ; package-version
  ;; #:use-module (gnu packages games)    ; steam-devices-udev-rules
)

;; no need to write: #:use-module (gnu services <module>)
(use-service-modules
 cups desktop networking ssh
 ;; lightdm  ; for lightdm-service-type
 ;; vnc      ; for xvnc-service-type
 ;; sddm     ; for sddm-service-type
 xorg     ; for gdm-service-type
 )

;; no need to write: #:use-module (gnu packages <module>)
(use-package-modules
 android  ; android-udev-rules - access smartphone via mtp://
 bash
 libusb   ; libmtp
 shells   ; login shell
 )

(define m (module-name-for-logging))
(evaluating-module)














































(define-public syst-config
  (operating-system
    (inherit (base:syst-config-linux))
    (keyboard-layout
     #;(operating-system-keyboard-layout (base:syst-config))
     (base:keyb-layout))
    (host-name host-geek)
    (users (base:users-config (list
                               "video"  ;; video devices, e.g. webcams
                               "lp"     ;; control bluetooth devices
                               )))
    ;; (locale "fr_FR.utf8")

;;; Packages installed system-wide. Users can also install packages under their
;;; own account: use 'guix search KEYWORD' to search for packages and 'guix
;;; install PACKAGE' to install a package.
    (packages
     (append
      (map specification->package
           (list
            "brightnessctl" #| backlight and LED brightness control |#
            ))












      (syst-packages-to-install)
      %base-packages))






    (services
     ;; TODO create macros pappend, premove, etc. - parallel processing
     (append
      (base:services)
      (list
       (set-xorg-configuration
        (xorg-configuration
         (keyboard-layout keyboard-layout)))




















       (udev-rules-service 'mtp libmtp) ;; mtp - Media Transfer Protocol
       (udev-rules-service 'android android-udev-rules
                           #:groups '("adbusers"))

       ;; Configure the Guix service and ensure we use Nonguix substitutes
       (simple-service 'add-nonguix-substitutes
                       guix-service-type
                       (guix-extension
                        (substitute-urls
                         (append (list "https://substitutes.nonguix.org")
                                 %default-substitute-urls))
                        (authorized-keys
;;; The signing-key.pub should be obtained by
;;;   wget https://substitutes.nonguix.org/signing-key.pub
                         (append (list (local-file "./signing-key.pub"))
                                 %default-authorized-guix-keys)))))

      ;; %desktop-services is the default list of services we are appending to.
      %desktop-services))







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
     (cons* (file-system
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

syst-config ;; operating-system (or image) must be returned
