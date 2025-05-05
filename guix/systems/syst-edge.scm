;; ## Initially:
;; ## 1. From another host:
;; # By default git is not installed in the raw Guix iso image / installation.
;; # Clone the repo to /tmp so that no uncommitted files (with secrets) are transferred
;; git clone $dotf /tmp/dotf && scp -r /tmp/dotf edge:/tmp
;;
;; ## 2. From the edge machine:
;; dotf=/tmp/dotf
;; mkdir -p ~/.config/guix
;; cp $dotf/.config/guix/channels.scm ~/.config/guix
;; # Make sure the ~/.config/guix/channels.scm contains only 'nonguix' and %default-channels
;; guix pull
;; sudo guix system --fallback -L $dotf/guix/common -L $dotf/guix/systems/common reconfigure $dotf/guix/systems/syst-$(hostname).scm
;;
;; ## Run this file by (the `~' doesn't work as a value of --load-path):
;; # --fallback         fall back to building when the substituter fails
;; # -L --load-path
;; sudo guix system --fallback -L $dotf/guix/common -L $dotf/guix/systems/common reconfigure $dotf/guix/systems/syst-$(hostname).scm

(define-module (syst-edge)
  #:use-module ((syst-base) #:prefix base:)
  #:use-module (settings)
  #:use-module (utils)                 ; for partial
  #:use-module (memo)
  #:use-module (cfg packages all)      ; for packages-to-install
  #:use-module (gnu)
  #:use-module (gnu system shadow)     ; for user-group; user-account-shell
  #:use-module (guix)                  ; for package-version
  ;; #:use-module (gnu packages games)    ; for steam-devices-udev-rules
)

;; no need to write: #:use-module (gnu services <module>)
(use-service-modules
 cups     ; printer
 desktop
 networking
 ssh

 ;; for lightdm-service-type - Run `lightdm', the LightDM graphical login
 ;; manager.
 lightdm

 ;; for xvnc-service-type - Run the Xvnc server, which creates a virtual X11
 ;; session and allow remote clients connecting to it via the remote framebuffer
 ;; (RFB) protocol.
 vnc

 ;; for sddm-service-type - SDDM, a display and log-in manager for X11 and
 ;; Wayland
 sddm

 ;; security-token ; usb card reader

 ;; for gdm-service-type - Run the GNOME Desktop Manager (GDM), which allows you
 ;; to log in in a graphical session, whether or not you use GNOME.
 xorg
 )

;; no need to write: #:use-module (gnu packages <module>)
(use-package-modules
 android  ; android-udev-rules - access smartphone via mtp://
 bash
 cups     ; lpinfo (printer)
 libusb   ; libmtp
 shells   ; login shell
 linux    ; brightnessctl
 )

(define m (module-name-for-logging))
(evaluating-module)

(define-public syst-config
  (operating-system
    (inherit (base:syst-config-linux))
    (keyboard-layout
     #;(operating-system-keyboard-layout (base:syst-config))
     (base:keyb-layout))
    (host-name host-edge)
    (users (base:users-config (list
                               "video"  ;; video devices, e.g. webcams
                               "lp"     ;; control bluetooth devices
                               )))
    (locale "fr_FR.utf8")

;;; Packages installed system-wide. Users can also install packages under their
;;; own account: use 'guix search KEYWORD' to search for packages and 'guix
;;; install PACKAGE' to install a package.
    (packages
     (append
      (list
       ;; backlight and LED brightness control
       brightnessctl)
      (packages-to-install)
      %base-packages))

    (services
     ;; TODO create macros pappend, premove, etc. - parallel processing
     (append
      (modify-services (base:services)
        (openssh-service-type
         config => (openssh-configuration
                    (inherit config)
                    ;; enable forwarding of X11 graphical client connections, so that ssh
                    ;; options -X and -Y work. However this doesn't fix the remote-desktop
                    ;; problem
                    ;; (x11-forwarding? #t)
                    )))

      (list
       ;; TODO lightdm doesn't work properly. The login fails
       ;; (service lightdm-service-type
       ;;          (lightdm-configuration
       ;;           (xorg-configuration
       ;;            (xorg-configuration (keyboard-layout (base:keyb-layout))))
       ;;           ;; (allow-empty-passwords? #t)

       ;;           ;; Not encrypted. More modern alternatives are VNC, RDP
       ;;           ;; (xdmcp? #t)

       ;;           ;; should be the TigerVNC for remote desktop access
       ;;           (vnc-server? #t)

       ;;           ;; (vnc-server-command
       ;;           ;;  (file-append tigervnc-server "/bin/Xvnc"
       ;;           ;;               "  -SecurityTypes None"))
       ;;           ;; (seats
       ;;           ;;  (list (lightdm-seat-configuration
       ;;           ;;         (name "*")
       ;;           ;;         (user-session "ratpoison"))))
       ;;           ))

       ;; (service pcscd-service-type)

       (service bluetooth-service-type)

       (service cups-service-type
                (cups-configuration
                 (web-interface? #t)
                 (extensions
                  (list cups-filters hplip-minimal))))

       ;; Use this when testing '[PATCH] doc: Improve VNC Services section.'
       ;; https://issues.guix.gnu.org/68453
       ;; (service xvnc-service-type (xvnc-configuration (display-number 5)))
       (service xvnc-service-type
                (xvnc-configuration
                 (display-number 5)
                 (geometry    ;; default: "1024x768"
                  "1920x1080"
                  ;; "2880x1620"
                  ;; "2880x1800"
                  ;; "2880x1800*"
                  )
                 ;; enable users to log in a desktop session from the login
                 ;; manager screen
                 (xdmcp? #t)  ;; default: #f
                 (inetd? #t)  ;; default: #f
                 ))

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
      (modify-services %desktop-services
        (gdm-service-type config => (gdm-configuration
                                     (inherit config)
                                     (auto-suspend? #f)
;;; See the Warning above in the xvnc-configuration
                                     (xdmcp? #t)))
        #;(delete gdm-service-type))))

;;; See
;;; https://guix.gnu.org/manual/en/html_node/Bootloader-Configuration.html
;;; https://www.gnu.org/software/grub/manual/grub/html_node/Invoking-grub_002dinstall.html#Invoking-grub_002dinstall
;;; https://github.com/babariviere/dotfiles/blob/guix/baba/bootloader/grub.scm
    (bootloader
     (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets (list "/boot/efi"))
      ;; keyboard-layout for the GRUB
      (keyboard-layout keyboard-layout)
      (menu-entries
       (list
        (let [(linux-version "6.11.0-25")]
          (menu-entry
           (label "Ubuntu 24.04.2 LTS")
           (linux (format #f "/boot/vmlinuz-~a-generic" linux-version))
           (linux-arguments
            ;; See /run/media/bost/c99384b9-.../boot/grub/grub.cfg
            '("root=UUID=c99384b9-1611-4318-a2d0-705e9b0c9007"
              "ro"     ; mount the root disk read only
              "quiet"  ; don't display console messages
              "splash" ; show a graphical "splash" screen while booting
              ))
           (initrd (format #f "/boot/initrd.img-~a-generic" linux-version))))

        (let [(linux-version "6.8.0-59")]
          (menu-entry
           (label "Linux Mint 22.1 Xia")
           (linux (format #f "/boot/vmlinuz-~a-generic" linux-version))
           (linux-arguments
            ;; See /media/bost/c99384b9-.../boot/grub/grub.cfg
            '("root=UUID=9025de3c-129b-46fe-bcde-4917d304e4ab"
              "ro"     ; mount the root disk read only
              "quiet"  ; don't display console messages
              "splash" ; show a graphical "splash" screen while booting
              ))
           (initrd (format #f "/boot/initrd.img-~a-generic" linux-version))))
        ))))

;;; The list of file systems that get "mounted". The unique file system
;;; identifiers there ("UUIDs") can be obtained by running 'blkid' in a
;;; terminal.
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

syst-config ;; operating-system (or image) must be returned
