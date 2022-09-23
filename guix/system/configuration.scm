;; This is an operating system configuration generated
;; by the graphical installer.

;; TODO compare /run/current-system/configuration.scm with
;; guix system describe | rg "configuration file" | rg -o "/gnu/.*"

#|
run this file by (the `~' doesn't work as a value of --load-path):
sudo guix system --load-path=$dotf/guix/system reconfigure $dotf/guix/system/configuration.scm
|#


(format #t "[configuration] evaluating ...\n")

(define-module (configuration)
  #:use-module (gnu)
  #:use-module (gnu packages libusb)   ; for libmtp
  #:use-module (gnu packages android)  ; for android-udev-rules
  #:use-module (gnu packages shells)   ; for login shell
  #:use-module (gnu system shadow)     ; for user-group; user-account-shell
  #:use-module (gnu packages bash)
  #:use-module (common settings))

(use-service-modules
  cups
  desktop
  networking
  ssh
  xorg)

(format #t "user-full-name: ~a\n" user-full-name)

(define operating-system-retval
  (operating-system
   (locale "en_US.utf8")
   (timezone "Europe/Berlin")
   (keyboard-layout
    (keyboard-layout
     "us,de,sk" "altgr-intl,,qwerty"
     #:options '("compose:menu,grp:ctrls_toggle")))
   (host-name "ecke")
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
            (comment user-full-name)
            (group "users")
            (home-directory "/home/bost")
            ;; login shell; see also `packages`
            ;; (shell (file-append fish "/bin/fish"))
            ;; explicitly define bash!
            (shell (file-append bash "/bin/bash"))
            (supplementary-groups
             '("wheel" "netdev" "audio" "video" "adbusers")))
           %base-user-accounts))
   (packages
    (append
     (map specification->package
          (list "emacs" "nss-certs"
                ;; "fish" ;; needed if it is the login-shell
                "vim"))
     %base-packages))
   #;
   (skeletons
   `((".guile" ,(plain-file "guile"
   "(use-modules (ice-9 readline))
   (activate-readline)"))))
   (services
    (append
     (list (service xfce-desktop-service-type)
           (service openssh-service-type)
           (service cups-service-type)
           (set-xorg-configuration
            (xorg-configuration
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
           (udev-rules-service 'mtp libmtp) ;; mtp - Media Transfer Protocol
           (udev-rules-service 'android android-udev-rules
                               #:groups '("adbusers")))
     %desktop-services))

   ;; see
   ;; https://guix.gnu.org/manual/en/html_node/Bootloader-Configuration.html
   ;; https://www.gnu.org/software/grub/manual/grub/html_node/Invoking-grub_002dinstall.html#Invoking-grub_002dinstall

   #|
   bash:
   udisksctl mount --block-device=$(blkid --uuid a8fb1680-eef5-49a0-98a3-8169c9b8eeda)
   sudo chmod +rw /tmp/grub.cfg
   sudo chown bost /tmp/grub.cfg
   sudo cp /media/bost/a8fb1680-eef5-49a0-98a3-8169c9b8eeda/boot/grub/grub.cfg /tmp/grub.cfg

   # guix show linux-libre | head | grep version | grep -oP "([0-9]{1,}\.)+[0-9]{1,}"
   guix system describe
   edit /tmp/grub.cfg
   sudo cp -i /tmp/grub.cfg /media/bost/a8fb1680-eef5-49a0-98a3-8169c9b8eeda/boot/grub/grub.cfg
   reboot with <f12> - edit boot order
   |#
   (bootloader
    (bootloader-configuration
     (bootloader grub-efi-bootloader)
     ;; (target "/boot/efi")
     (targets
      '("/boot/efi1"))
     (menu-entries
      (list
       (let ((linux-version "5.15.0-47"))
         (menu-entry
          (label "Ubuntu")
          (linux (format #t "/boot/vmlinuz-~a-generic" linux-version))
          ;; ro - mount the root disk read only.
          ;; quiet - don’t display console messages
          ;; splash - show a graphical "splash" screen while booting.
          (linux-arguments '("root=UUID=a8fb1680-eef5-49a0-98a3-8169c9b8eeda"
                             "ro" "quiet" "splash"
                             ;; value $vt_handoff is "vt.handoff=7" or unspecified
                             #;"$vt_handoff"))
          (initrd (format #t "/boot/initrd.img-~a-generic" linux-version))))))

     (keyboard-layout keyboard-layout)))
   (file-systems
    (cons* (file-system
            (mount-point "/")
            (device
             (uuid "67ce5d9c-7af1-4435-a2a9-68651ab9a281"
                   'ext4))
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
   (swap-devices
    (list
     (swap-space (target "/swapfile"))))
   ))

(format #t "\n[configuration] evaluated\n")
;; operating-system (or image) must be returned
operating-system-retval
