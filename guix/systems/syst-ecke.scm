(define-module (syst-ecke)
  #:use-module ((syst-base) #:prefix base:)
  #:use-module (settings)
  #:use-module (utils)                 ; for partial
  #:use-module (memo)
  #:use-module (cfg packages all)      ; for packages-to-install
  #:use-module (gnu)
  #:use-module (gnu system shadow)     ; for user-group; user-account-shell
  #:use-module (guix)                  ; for package-version
  ;; #:use-module (nongnu packages linux)
  ;; #:use-module (nongnu system linux-initrd)
)

;; no need to write: #:use-module (gnu services <module>)
(use-service-modules
 cups desktop networking ssh
 xorg     ; for gdm-service-type
 sddm     ; for sddm-service-type
 )

;; no need to write: #:use-module (gnu packages <module>)
(use-package-modules
 android  ; android-udev-rules - access smartphone via mtp://
 bash
 libusb   ; libmtp
 shells   ; login shell

 ;; vim

 ;; Window Managers:
 ;; - sway
 ;; - herbstluftwm
 ;; - i3-wm
 ;; - xmonad (Tiling window manager)
 ;; ...
 wm ;; provides "sway" "swaybg" "swayidle" "swaylock"

 ;; video
 ;; certs
 ;; version-control
 ;; terminals
 ;; disk
 ;; xdisorg
 ;; web-browsers
 )

(evaluating-module)

(define-public syst-config
  (operating-system
    (inherit base:syst-config)
    ;; keyboard-layout is not inherited
    (keyboard-layout base:keyb-layout)
    ;; (keyboard-layout (operating-system-keyboard-layout base:syst-config))

    (host-name host-ecke)
    (users (base:users-config (list
                               "video"  ;; video devices, e.g. webcams
                               )))

    (locale "fr_FR.utf8")

;;; Packages installed system-wide. Users can also install packages under their
;;; own account: use 'guix search KEYWORD' to search for packages and 'guix
;;; install PACKAGE' to install a package.
    (packages
     (append
      (map specification->package
;;; # Get the sway configuration file:
;;; mkdir -p $dotf/.config/sway
;;; # -O/--output-document works only if the given output file does not exist.
;;; wget https://raw.githubusercontent.com/swaywm/sway/master/config.in \
;;;      --output-document=$dotf/.config/sway/config

;;; Crafting a Minimal Sway Environment with Guix - System Crafters Live!
;;; https://www.youtube.com/live/OYbenLOm3Js?feature=share&t=5122
           (list "sway" "swaybg" "swayidle" "swaylock"
                 "bemenu"   ;; Dynamic menu library and client program inspired by dmenu
                 "ranger"   ;; Minimalistic console file manager with Vi key bindings
                 #;"luakit" ;; Simple browser extensible by Lua based on WebKit & GTK+ toolkit
                 #;"mpv"    ;; Audio and video player
                 "termite"  ;; Minimal terminal emulator. Designed for use with tiling window managers.
                 ))
      packages-to-install
      %base-packages))
    #;
    (skeletons
    `((".guile" ,(plain-file "guile"
    "(use-modules (ice-9 readline))
    (activate-readline)"))))
    (services
     ;; TODO create macros pappend, premove, etc.

     (append
      base:services
      (list
       (set-xorg-configuration
        (xorg-configuration
         (keyboard-layout keyboard-layout))
        sddm-service-type)
       (service gnome-desktop-service-type)
       (service mate-desktop-service-type)
       (service cups-service-type)
;;; See https://git.sr.ht/~krevedkokun/dotfiles/tree/master/item/system/desktop.scm and/or
;;; https://github.com/nicolas-graves/dotfiles/blob/c91d5a0e29b631a1fa9720c18a827a71ffb66033/System.org
;;; `udev-rules-service' is more convenient than using ‘modify-services’ & co.
;;; see https://issues.guix.gnu.org/40454
       ;; (modify-services base:services
       ;;                  (udev-service-type
       ;;                   config =>
       ;;                   (udev-configuration
       ;;                    (inherit config)
       ;;                    (rules (cons*
       ;;                            light
       ;;                            pipewire-0.3
       ;;                            android-udev-rules
       ;;                            libu2f-host
       ;;                            (udev-configuration-rules config))))))

       ;; mtp - Media Transfer Protocol
       (udev-rules-service 'mtp libmtp)
       (udev-rules-service 'android android-udev-rules
                           #:groups '("adbusers")))

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
                             %default-authorized-guix-keys))))
        ;; for sway - see the patch:
        ;;   Add a guide to the guix cookbook about setting up sway.
        ;;   https://issues.guix.gnu.org/issue/39271
        (delete gdm-service-type))))

;;; See
;;; https://guix.gnu.org/manual/en/html_node/Bootloader-Configuration.html
;;; https://www.gnu.org/software/grub/manual/grub/html_node/Invoking-grub_002dinstall.html#Invoking-grub_002dinstall
;;; https://github.com/babariviere/dotfiles/blob/guix/baba/bootloader/grub.scm
    (bootloader
     (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets '("/boot/efi1")) ;; (target "/boot/efi")
      ;; keyboard-layout for the GRUB
      (keyboard-layout keyboard-layout)
      (menu-entries
       (list
        (let ((linux-version "6.5.0-21"))
          (menu-entry
           (label "Ubuntu")
           (linux (format #f "/boot/vmlinuz-~a-generic" linux-version))
           ;; ro - mount the root disk read only.
           ;; quiet - don’t display console messages
           ;; splash - show a graphical "splash" screen while booting.
           (linux-arguments '("root=UUID=fe7ecf10-c42e-4bbf-b377-3a9346088e63"
                              "ro" "quiet" "splash"
                              ;; value $vt_handoff is "vt.handoff=7" or
                              ;; unspecified
                              #;"$vt_handoff"))
           (initrd (format #f "/boot/initrd.img-~a-generic" linux-version))))))))

    (file-systems
     (cons* (file-system
              (mount-point "/boot/efi1")
              (device (uuid "DC78-41C2" 'fat32))
              (type "vfat"))
            (file-system
              (mount-point "/")
              (device (uuid "67ce5d9c-7af1-4435-a2a9-68651ab9a281" 'ext4))
              (type "ext4"))
            ;; (file-system
            ;;   (mount-point "/boot/efi2")
            ;;   (device (uuid "B513-9669" 'fat32))
            ;;   (type "vfat"))
            %base-file-systems))

    ;; warning: List elements of the field 'swap-devices' should now use the
    ;; <swap-space> record, as the old method is deprecated.
    ;; See "(guix) operating-system Reference" for more details.
    (swap-devices (list
                   (swap-space (target "/swapfile"))))))

(module-evaluated)

;; and operating-system (or image) must be returned
syst-config
