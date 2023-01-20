;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.


;; Indicate which modules to import to access the variables
;; used in this configuration.
(define-module (systems lukas)
  #:use-module (gnu)
  ;; #:use-module (gnu system shadow)     ; for user-group; user-account-shell
  ;; #:use-module (common settings)
  )

(use-service-modules
 cups
 desktop
 networking
 ssh
 xorg)

(operating-system
  (locale "en_US.utf8")
  (timezone "Europe/Berlin")
  (keyboard-layout (keyboard-layout "us" "altgr-intl"))
  (host-name "lukas")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "bost")
                  (comment "Rostislav Svoboda")
                  (group "users")
                  (home-directory "/home/bost")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))
  (packages (append (list (specification->package "nss-certs"))
                    %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (append (list (service xfce-desktop-service-type)

                 ;; To configure OpenSSH, pass an 'openssh-configuration'
                 ;; record as a second argument to 'service' below.
                 (service openssh-service-type)
                 (set-xorg-configuration
                  (xorg-configuration (keyboard-layout keyboard-layout))))

           ;; This is the default list of services we
           ;; are appending to.
           %desktop-services))
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets (list "/dev/sda"))
                (keyboard-layout keyboard-layout)))
  (swap-devices (list (swap-space
                        (target (uuid
                                 "a4767437-a9c8-4d57-9755-4fcd2aef73da")))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid
                                  "cf11628d-4887-42d2-aef1-635ad5089ce1"
                                  'ext4))
                         (type "ext4")) %base-file-systems)))
