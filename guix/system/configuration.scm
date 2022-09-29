;; This is an operating system configuration generated
;; by the graphical installer.

;; TODO compare /run/current-system/configuration.scm with
;; guix system describe | rg "configuration file" | rg -o "/gnu/.*"

#|
# run this file by (the `~' doesn't work as a value of --load-path):
# --fallback         fall back to building when the substituter fails
sudo guix system --fallback --load-path=$dotf/guix/system reconfigure $dotf/guix/system/configuration.scm

udisksctl mount --block-device=(blkid --uuid a8fb1680-eef5-49a0-98a3-8169c9b8eeda)
sudo cp /media/$USER/a8fb1680-eef5-49a0-98a3-8169c9b8eeda/boot/grub/grub.cfg /tmp/grub.cfg
sudo chown $USER /tmp/grub.cfg && sudo chmod +rw /tmp/grub.cfg
<edit /tmp/grub.cfg>
# grep -oP "([0-9]{1,}\.)+[0-9]{1,}" # match the version number
rg --no-line-number -A 4 --max-count=1 "GNU with Linux-Libre" /boot/grub/grub.cfg
<copy the block>
guix system describe | rg current
<extract the time and generation number>
sudo cp /tmp/grub.cfg /media/$USER/a8fb1680-eef5-49a0-98a3-8169c9b8eeda/boot/grub/grub.cfg
sudo reboot # press <f12> during the reboot and fix the boot order
|#

(format #t "[configuration] evaluating ...\n")

(define-module (configuration)
  #:use-module (gnu)
  #:use-module (gnu system shadow)     ; for user-group; user-account-shell
  #:use-module (common settings))

(use-service-modules
  cups
  desktop
  networking
  ssh
  xorg)

;; 'use-package-modules' is unlike the general 'use-modules' specifically for
;; packages. Every package module from 'use-package-modules' can be included under
;; the 'use-modules'.
(use-package-modules
 android  ; for android-udev-rules - access smartphone via mtp://
 bash
 libusb   ; for libmtp
 shells   ; for login shell
 )


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
             '("wheel" ;; gives access to 'sudo'
               "netdev" "audio" "video" "adbusers")))
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
     (modify-services
      %desktop-services
      (guix-service-type
       config =>
       (guix-configuration
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

   (bootloader
    (bootloader-configuration
     (bootloader grub-efi-bootloader)
     (targets '("/boot/efi1")) ;; (target "/boot/efi")
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
                             ;; value $vt_handoff is "vt.handoff=7" or
                             ;; unspecified
                             #;"$vt_handoff"))
          (initrd (format #t "/boot/initrd.img-~a-generic" linux-version))))))

     (keyboard-layout keyboard-layout)))
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

(format #t "\n[configuration] evaluated\n")
;; operating-system (or image) must be returned
operating-system-retval
