;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.


;; Indicate which modules to import to access the variables
;; used in this configuration.

#|
## Run this file by (the `~' doesn't work as a value of --load-path):
dotf=$HOME/dev/dotfiles
sudo guix system -L $dotf/guix/systems reconfigure $dotf/guix/systems/lukas.scm
|#
(define-module (lukas)
  #:use-module (gnu)
  #:use-module (gnu services vnc)
  ;; #:use-module (gnu system shadow)     ; for user-group; user-account-shell
  ;; #:use-module (common settings)
  )

(use-service-modules
 cups
 desktop
 networking
 ssh
 xorg)

(define disable-suspend-srvc
;;; GDM auto-suspend? is not system-wide and can be overriden by users. Create a
;;; system-wide local dconf profile. See https://paste.centos.org/view/568513c6
  (simple-service
   'disable-auto-suspend dconf-service-type
   (list (dconf-profile
          (name "local")
          (content '("system-db:local"))
          (keyfile (dconf-keyfile
                    (name "00-disable-suspend")
                    (content
                     '("[org/gnome/settings-daemon/plugins/power]"
                       "sleep-inactive-ac-type='nothing'"
                       "sleep-inactive-battery-type='nothing'"
                       "sleep-inactive-ac-timeout=0"
                       "sleep-inactive-battery-timeout=0"))))))))

(operating-system
  (locale "en_US.utf8")
  (timezone "Europe/Berlin")
  (keyboard-layout (keyboard-layout "us" "altgr-intl"))
  (host-name host-lukas)

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "bost")
                  (comment "Rostislav Svoboda")
                  (group "users")
                  (home-directory "/home/bost")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))
  (packages (append (list
                     (specification->package "nss-certs")
;;; Install git system-wide so that I can do `git clone '
                     (specification->package "git"))
                    %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (append (list (service xfce-desktop-service-type)

                 ;; To configure OpenSSH, pass an 'openssh-configuration'
                 ;; record as a second argument to 'service' below.
                 (service openssh-service-type)
                 (set-xorg-configuration
                  (xorg-configuration (keyboard-layout keyboard-layout)))
                 (service xvnc-service-type (xvnc-configuration
                                             (display-number 5)
;;; Warning: Unless your machine is in a controlled environment, for security
;;; reasons, the localhost? configuration of the xvnc-configuration record
;;; should be left to its default #t value and exposed via a secure means such
;;; as an SSH port forward. The XDMCP port, UDP 177 should also be blocked from
;;; the outside by a firewall, as it is not a secure protocol and can expose
;;; login credentials in clear.
                                             ;; (localhost? #f)
                                             ;; (xdmcp? #t)
;;; Use an Inetd-style service, which runs the Xvnc server on demand.
;;; (default: #f)
                                             (inetd? #t)))
                 ;; disable-suspend-srvc
                 )

           ;; This is the default list of services we are appending to.
           (modify-services %desktop-services
                            (gdm-service-type config => (gdm-configuration
                                                         (inherit config)
                                                         (auto-suspend? #f)
;;; See the Warning above in the xvnc-configuration
                                                         ;; (xdmcp? #t)
                                                         )))))
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
