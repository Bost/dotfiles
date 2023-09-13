(define-module (syst-lukas)
  #:use-module (settings)
  #:use-module (utils)                 ; for partial
  #:use-module (memo)

  #:use-module (gnu)
  ;; #:use-module (gnu system shadow)  ; for user-group; user-account-shell
  #:use-module (guix)                  ; for package-version
  #:export (
            syst-config
            ))

;; no need to write: #:use-module (gnu services <module>)
(use-service-modules desktop networking ssh
                     vnc
                     xorg)

(define m (module-name-for-logging))
;; (format #t "~a evaluating module ...\n" m)

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

(define syst-config
  (operating-system
    (locale "en_US.utf8")
    (timezone "Europe/Berlin")
    (keyboard-layout ; keyboard-layout for the console
     (keyboard-layout "us" "altgr-intl"))
    (host-name host-lukas)

;;; The list of user accounts ('root' is implicit).
    (users (cons*
            (user-account
             (name "bost")
             (comment
              (begin
                ;; (format #t "~a user-full-name: ~a\n" m user-full-name)
                user-full-name))
             (group "users")
             (home-directory "/home/bost")

             ;; list of group names that this user-account belongs to
             (supplementary-groups
              ;; grant access to:
              '("wheel"  #| sudo etc. |#
                "netdev" #| network devices |#
                "audio"  #| sound card |#
                "cdrom"  #| access to CD-ROM |#)))
            %base-user-accounts))
;;; Packages installed system-wide. Users can also install packages under their
;;; own account: use 'guix search KEYWORD' to search for packages and 'guix
;;; install PACKAGE' to install a package.
    (packages (append
               (map specification->package
                    (list
;;; Install git & rsync system-wide to be able to git-clone / rsync the dotfiles
                     "git"
                     "gparted"    #| disk partition |#
                     "nss-certs"
                     "rsync"      #| 'scp' is preinstalled |#
                     "vim"        #| 'vi' is preinstalled |#
                     ))
               %base-packages))

    ;; Below is the list of system services.  To search for available
    ;; services, run 'guix system search KEYWORD' in a terminal.
    (services
     (append (list
              (service xfce-desktop-service-type)

              ;; ntp-service-type for system clock sync is in the
              ;; %desktop-services by default

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
                           (type "ext4")) %base-file-systems))))

((compose
  (partial format #t "~a kernel-version: ~a\n" m)
  package-version
  operating-system-kernel)
 syst-config)

;; (format #t "~a module evaluated\n" m)

;; operating-system (or image) must be returned
syst-config
