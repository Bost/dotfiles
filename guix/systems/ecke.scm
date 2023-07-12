;; TODO compare /run/current-system/configuration.scm with
;; guix system describe | rg "configuration file" | rg -o "/gnu/.*"

;;; To run this file see: $dotf/bin/sgxsr

(define-module (ecke)
  #:use-module (settings)
  #:use-module (utils)                 ; for partial
  #:use-module (memo)

  #:use-module (gnu)
  #:use-module (gnu system shadow)     ; for user-group; user-account-shell
  #:use-module (guix)                  ; for package-version
  #:use-module (srfi srfi-1)           ; for remove
  ;; #:use-module (gnu services xorg)     ; for gdm-service-type
  )

;; no need to write: #:use-module (gnu services <module>)
(use-service-modules cups desktop networking ssh xorg)

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


(when (is-system-ecke)
  (let* []
    (define m (module-name-for-logging))
    ;; (format #t "~a evaluating module ...\n" m)

    (define operating-system-configuration
      (operating-system
        (locale "en_US.utf8")
        (timezone "Europe/Berlin")
        (keyboard-layout ; keyboard-layout for the console
         (keyboard-layout
          "us,de,sk,fr" "altgr-intl,,qwerty,"
          #:options '("compose:menu,grp:ctrls_toggle")))
        (host-name host-ecke)

;;; The list of user accounts ('root' is implicit).
        (users (cons*
                ;; Password for some new <user> must be set by 'sudo passwd <user>'. See
                ;; https://guix.gnu.org/manual/en/html_node/User-Accounts.html
                ;; for 'guix home' I had to create /var/guix/profiles/per-user/<user>
                ;;   sudo mkdir /var/guix/profiles/per-user/<user>
                ;;   sudo chown -R <user>:users /var/guix/profiles/per-user/<user>
                #|
                (user-account
                (name "foo")
                (comment "Foo")
                (group "users")
                (home-directory "/home/foo")
                ;; login shell declaration. See also `packages` and:
                ;;   ~/dev/guix/gnu/home/services/shells.scm
                ;;   ~/dev/guix/gnu/system/shadow.scm
                ;;   ~/dev/guix/guix/build/r-build-system.scm
                ;;   ~/dev/guix/tests/store.scm
                ;;   ~/dev/guix/guix/build/haskell-build-system.scm
                ;;   ~/dev/guix/guix/scripts/environment.scm
                ;;   ~/dev/guix/guix/scripts/home.scm
                ;;   ~/dev/guix/guix/build/gnu-build-system.scm
                ;;   ~/dev/guix/guix/build/utils.scm
                ;;   ~/dev/guix/guix/build/emacs-build-system.scm
                ;; (shell (file-append fish "/bin/fish")))
                |#

                (user-account
                 (name "bost")
                 (comment
                  (begin
                    ;; (format #t "~a user-full-name: ~a\n" m user-full-name)
                    user-full-name))
                 (group "users")
                 (home-directory "/home/bost")
                 ;; login shell; see also `packages`
                 ;; explicitly define fish / bash:
                 ;; (shell (file-append fish "/bin/fish"))
                 (shell (file-append bash "/bin/bash"))

                 ;; list of group names that this user-account belongs to
                 (supplementary-groups
                  ;; grant access to:
                  '("wheel"  #| sudo etc.; See polkit-wheel-service for administrative tasks for non-root users |#
                    "netdev" #| network devices |#
                    "audio"  #| sound card |#
                    "video"  #| video devices, e.g. webcams |#)))
                %base-user-accounts))

;;; Packages installed system-wide. Users can also install packages under their
;;; own account: use 'guix search KEYWORD' to search for packages and 'guix
;;; install PACKAGE' to install a package.
        (packages
         (append
          (map specification->package
               (append
                (list "sway" "swaybg" "swayidle" "swaylock"
                      #;"bemenu" #| Dynamic menu library and client program inspired by dmenu |#
			                #;"ranger" #| minimalistic console file manager with Vi key bindings |#
			                #;"luakit" #| simple browser extensible by Lua based on WebKit & GTK+ toolkit |#
			                #;"mpv"    #| Audio and video player |#)
                (list
                 "git"
                 #;"gparted" #|  disk partition |#
                 "nss-certs"
                 #;"rsync"  #| 'scp' is preinstalled |#
                 #;"vim" #| 'vi' is preinstalled |#)))
          %base-packages))
        #;
        (skeletons
        `((".guile" ,(plain-file "guile"
        "(use-modules (ice-9 readline))
        (activate-readline)"))))
        (services
         ((compose
           ;; TODO create macros pappend, premove, etc.
           (partial remove (lambda (service)
		                         (eq? (service-kind service) gdm-service-type)))
           (partial
            append
            (list
             (service xfce-desktop-service-type)

             ;; To configure OpenSSH, pass an 'openssh-configuration'
             ;; record as a second argument to 'service' below.
             (service openssh-service-type)

             ;; ntp-service-type for system clock sync is in the
             ;; %desktop-services by default

             (service cups-service-type)
             (set-xorg-configuration
              (xorg-configuration ; keyboard-layout for the XOrg
               (keyboard-layout keyboard-layout)))
             ;; (udev-rules-service 'mtp libmtp)
             ;; See https://git.sr.ht/~krevedkokun/dotfiles/tree/master/item/system/desktop.scm and/or
             ;; https://github.com/nicolas-graves/dotfiles/blob/c91d5a0e29b631a1fa9720c18a827a71ffb66033/System.org
             ;; `udev-rules-service' is more convenient than using ‘modify-services’ & co.
             ;; see https://issues.guix.gnu.org/40454
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
                                 #:groups '("adbusers")))))
          (modify-services
           %desktop-services
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

        ;; see
        ;; https://guix.gnu.org/manual/en/html_node/Bootloader-Configuration.html
        ;; https://www.gnu.org/software/grub/manual/grub/html_node/Invoking-grub_002dinstall.html#Invoking-grub_002dinstall
        ;; https://github.com/babariviere/dotfiles/blob/guix/baba/bootloader/grub.scm

        (bootloader
         (bootloader-configuration
          (bootloader grub-efi-bootloader)
          (targets '("/boot/efi1")) ;; (target "/boot/efi")
          (menu-entries
           (list
            (let ((linux-version "5.15.0-47"))
              (menu-entry
               (label "Ubuntu")
               (linux (format #f "/boot/vmlinuz-~a-generic" linux-version))
               ;; ro - mount the root disk read only.
               ;; quiet - don’t display console messages
               ;; splash - show a graphical "splash" screen while booting.
               (linux-arguments '("root=UUID=a8fb1680-eef5-49a0-98a3-8169c9b8eeda"
                                  "ro" "quiet" "splash"
                                  ;; value $vt_handoff is "vt.handoff=7" or
                                  ;; unspecified
                                  #;"$vt_handoff"))
               (initrd (format #f "/boot/initrd.img-~a-generic" linux-version))))))
          (keyboard-layout keyboard-layout))) ; keyboard-layout for the GRUB

        (file-systems
         (cons* (file-system
                  (mount-point "/")
                  (device (uuid "67ce5d9c-7af1-4435-a2a9-68651ab9a281" 'ext4))
                  (type "ext4"))
                (file-system
                  (mount-point "/boot/efi1")
                  (device (uuid "DC78-41C2" 'fat32))
                  (type "vfat"))
                ;; (file-system
                ;;   (mount-point "/boot/efi2")
                ;;   (device (uuid "B513-9669" 'fat32))
                ;;   (type "vfat"))
                %base-file-systems))
        ;; warning: List elements of the field 'swap-devices' should now use the
        ;; <swap-space> record, as the old method is deprecated.
        ;; See "(guix) operating-system Reference" for more details.
        (swap-devices (list (swap-space (target "/swapfile"))))))

    ((compose
      (partial format #t "~a kernel-version: ~a\n" m)
      package-version
      operating-system-kernel)
     operating-system-configuration)

    #;
    (format #t
            (str
             "~a module evaluated\n"
             "\n~a")
            m
            operating-system-configuration)

    ;; operating-system (or image) must be returned
    operating-system-configuration))
