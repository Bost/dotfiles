(define-module (syst-ecke)
  #:use-module ((syst-base) #:prefix base:)
  #:use-module (settings)
  #:use-module (utils)                  ; partial
  #:use-module (memo)
  #:use-module (ice-9 pretty-print)
  #:use-module (cfg packages all)       ; packages-to-install
  #:use-module (gnu)
  #:use-module (gnu system shadow)      ; user-group user-account-shell
  #:use-module (guix)                   ; package-version
  #:use-module (gnu packages games)     ; steam-devices-udev-rules
  #:use-module (nongnu packages nvidia) ; replace-mesa nvda
  #:use-module (nongnu services nvidia) ; nvidia-service-type
)

;; no need to write: #:use-module (gnu services <module>)
(use-service-modules
 cups     ; printer
 desktop
 networking
 ssh
 linux    ; kernel-module-loader-service-type
 ;; lightdm  ; for lightdm-service-type
 ;; vnc      ; for xvnc-service-type
 sddm     ; for sddm-service-type
 ;; security-token ; usb card reader
 xorg     ; for gdm-service-type
 )

;; no need to write: #:use-module (gnu packages <module>)
(use-package-modules
 android  ; android-udev-rules - access smartphone via mtp://
 cups     ; lpinfo (printer)
 bash
 fonts    ; font-terminus font-tamzen
 gnome    ; for (gnome-desktop-configuration (gnome (replace-mesa gnome)))
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

(define m (module-name-for-logging))
(evaluating-module)

(define (sway-package-specifications)
;;; # Get the sway configuration file:
;;; mkdir -p $dotf/.config/sway
;;; # -O/--output-document works only if the given output file does not exist.
;;; wget https://raw.githubusercontent.com/swaywm/sway/master/config.in \
;;;      --output-document=$dotf/.config/sway/config

;;; Crafting a Minimal Sway Environment with Guix - System Crafters Live!
;;; https://www.youtube.com/live/OYbenLOm3Js?feature=share&t=5122
  (list
   "sway" "swaybg" "swayidle" "swaylock"
   "bemenu"   ;; Dynamic menu library and client program inspired by dmenu
   "ranger"   ;; Minimalistic console file manager with Vi key bindings
   #;"luakit" ;; Simple browser extensible by Lua based on WebKit & GTK+ toolkit
   #;"mpv"    ;; Audio and video player
   "termite"  ;; Minimal terminal emulator for use with tiling window managers
   ))

(define (service-list)
  "TODO create macros pappend, premove, etc. - parallel processing"
  ((comp
    (lambda (lst)
      (format #t "\n")
      (format #t "ecke ~a (length lst) ~a\n" m (length lst))
      (map (partial format #t "ecke ~a ~a\n" m) (map base:get-service-type lst))
      ;; (pretty-print lst)
      ;; (for-each pretty-print (map service-type lst))
      (format #t "\n")
      lst))
   (append
    (base:services)
    (list
     ;; (service pcscd-service-type) ;; usb card reader
     (service nvidia-service-type)

     ;; Configure desktop environment, GNOME for example.
     (service gnome-desktop-service-type
              ;; Enable NVIDIA support, only do this when the card is
              ;; used for displaying.
              ;; Adding the following leads to
              ;;   profile contains conflicting entries for packagekit
              #;
              (gnome-desktop-configuration
              (gnome (replace-mesa gnome))))

     ;; Configure Xorg server, only do this when the card is used for
     ;; displaying.
     (set-xorg-configuration
      (xorg-configuration
       (modules (cons nvda %default-xorg-modules))
       (drivers '("nvidia"))
       (keyboard-layout keyboard-layout))
      sddm-service-type)

     ;; (service mate-desktop-service-type)

     (service cups-service-type
              (cups-configuration
               (web-interface? #t)
               (extensions
                (list cups-filters hplip-minimal))))
;;; See https://git.sr.ht/~krevedkokun/dotfiles/tree/master/item/system/desktop.scm and/or
;;; https://github.com/nicolas-graves/dotfiles/blob/c91d5a0e29b631a1fa9720c18a827a71ffb66033/System.org
;;; `udev-rules-service' is more convenient than using ‘modify-services’ & co.
;;; see https://issues.guix.gnu.org/40454
     ;; (modify-services (base:services)
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

     (udev-rules-service 'mtp libmtp) ;; mtp - Media Transfer Protocol
     (udev-rules-service 'android android-udev-rules
                         #:groups '("adbusers"))
     (udev-rules-service 'steam-devices steam-devices-udev-rules)

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
                               %default-authorized-guix-keys))))

     ;; ;; Configure TTYs and graphical greeter
     ;; (service
     ;;  console-font-service-type
     ;;  `(("tty1" . "LatGrkCyr-8x16")
     ;;    ("tty2" . ,(file-append
     ;;                font-tamzen
     ;;                "/share/kbd/consolefonts/TamzenForPowerline10x20.psf"))
     ;;    ("tty3" . ,(file-append
     ;;                font-terminus
     ;;                "/share/consolefonts/ter-132n")))

     ;;  ;; Larger font for HIDPI screens, however this is too large
     ;;  ;; (map (lambda (tty)
     ;;  ;;        (cons tty (file-append
     ;;  ;;                   font-terminus
     ;;  ;;                   "/share/consolefonts/ter-132n")))
     ;;  ;;      '("tty1" "tty2" "tty3"))
     ;;  )

     ;; (service greetd-service-type
     ;;          (greetd-configuration
     ;;           (greeter-supplementary-groups (list "video" "input"))
     ;;           (terminals
     ;;            (list
     ;;             ;; TTY1 is the graphical login screen for Sway
     ;;             (greetd-terminal-configuration
     ;;              (terminal-vt "1")
     ;;              (terminal-switch #t))

     ;;             ;; Set up remaining TTYs for terminal use
     ;;             (greetd-terminal-configuration (terminal-vt "2"))
     ;;             (greetd-terminal-configuration (terminal-vt "3"))))))

     (simple-service
      'custom-udev-rules udev-service-type
      (list nvidia-driver))

     ;; load loadable kernel modules at boot with modprobe
     (service kernel-module-loader-service-type
              '("ipmi_devintf"
                "nvidia"
                "nvidia_modeset"
                "nvidia_uvm"))

     ;; Configure swaylock as a setuid program
     ;; (service screen-locker-service-type
     ;;          (screen-locker-configuration
     ;;           (name "swaylock")
     ;;           (program (file-append swaylock "/bin/swaylock"))
     ;;           (using-pam? #t)
     ;;           (using-setuid? #f)))
     )

    ;; %desktop-services is the default list of services we are appending to.
    (modify-services %desktop-services
      ;; (delete login-service-type)
      ;; (delete mingetty-service-type)
      ;; (delete console-font-service-type)
      ;; for sway - see the patch:
      ;;   Add a guide to the guix cookbook about setting up sway.
      ;;   https://issues.guix.gnu.org/issue/39271

      ;; service "xorg-server" must be defined only once (see above)
      (delete gdm-service-type)))))

(define-public syst-config
  (operating-system
    (inherit (base:syst-config-linux))
    (keyboard-layout
     #;(operating-system-keyboard-layout (base:syst-config))
     (base:keyb-layout))
    (host-name host-ecke)
    (users (base:users-config (list
                               "video"  ;; video devices, e.g. webcams
                               ;; "lp"     ;; control bluetooth devices
                               )))
    (locale "fr_FR.utf8")

;;; Packages installed system-wide. Users can also install packages under their
;;; own account: use 'guix search KEYWORD' to search for packages and 'guix
;;; install PACKAGE' to install a package.
    (packages
     (append
      (map specification->package (sway-package-specifications))
      (list)
      (packages-to-install)
      %base-packages))

    ;; Tweaks for nvidia drivers to work
    (kernel-arguments '("modprobe.blacklist=nouveau"
                        ;; Set this if the card is not used for displaying or
                        ;; you're using Wayland:
                        "nvidia_drm.modeset=1"))

    ;; (skeletons
    ;; `((".guile" ,(plain-file
    ;;               "guile"
    ;;               "(use-modules (ice-9 readline)) (activate-readline)"))))

    (services (service-list))

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
        (let ((linux-version "6.11.0-13"))
          (menu-entry
           (label "Ubuntu 24.10")
           ;; vmlinuz - compressed linux kernel
           (linux (format #f "/boot/vmlinuz-~a-generic" linux-version))
           (linux-arguments
            '("root=UUID=fe7ecf10-c42e-4bbf-b377-3a9346088e63"
              "ro"     ; mount the root filesystem as read only
              "quiet"  ; don't display console messages
              "splash" ; show a graphical "splash" screen while booting

              ;; Configure the kexec-based crash kernel. It reserves memory for
              ;; a secondary kernel used in case the main kernel crashes.
              "crashkernel=2G-4G:320M,4G-32G:512M,32G-64G:1024M,64G-128G:2048M,128G-:4096M"

              ;; In Ubuntu’s grub.cfg, $vt_handoff is a placeholder for GRUB to
              ;; dynamically manage terminal-to-graphical handoff. However, this
              ;; mechanism is unnecessary in Guix because the arguments are
              ;; passed directly to the kernel without relying on GRUB-specific
              ;; scripting features.
              ;;
              ;; If you explicitly include $vt_handoff in your Guix
              ;; linux-arguments, it will be passed as a literal string, which
              ;; the kernel won’t understand, potentially causing issues.
              #;"$vt_handoff"))
           (initrd
            (format #f "/boot/initrd.img-~a-generic" linux-version))))))))

;;; The list of file systems that get "mounted". The unique file system
;;; identifiers there ("UUIDs") can be obtained by running 'blkid' in a
;;; terminal.
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
    ;;
    ;; Use the file '/swapfile' as swap space, which depends on the file system
    ;; mounted at '/'
    (swap-devices
     (list
      (swap-space
       (target "/swapfile")
       (dependencies (filter (file-system-mount-point-predicate "/")
                             file-systems)))))))

(module-evaluated)

syst-config ;; operating-system (or image) must be returned
