(define-module (syst-lukas)
  #:use-module ((syst-base) #:prefix base:)
  #:use-module (settings)
  #:use-module (utils)                 ; for partial
  #:use-module (memo)
  #:use-module (cfg packages all)      ; for packages-to-install
  #:use-module (gnu)

  #:use-module (guix)                  ; for package-version

)

;; no need to write: #:use-module (gnu services <module>)
(use-service-modules
 #;cups desktop networking ssh
 vnc      ; for xvnc-service-type
 xorg     ; for gdm-service-type
 )





























(define m (module-name-for-logging))
(evaluating-module)

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











(define-public syst-config
  (operating-system
    (inherit (base:syst-config))
    (keyboard-layout

     (keyboard-layout "us" "altgr-intl"))
    (host-name host-lukas)
    (users (base:users-config (list
                               "cdrom" ;; access to CD-ROM

                               )))


;;; Packages installed system-wide. Users can also install packages under their
;;; own account: use 'guix search KEYWORD' to search for packages and 'guix
;;; install PACKAGE' to install a package.
    (packages
     (append
















      (packages-to-install)
      %base-packages))






    (services
     ;; TODO create macros pappend, premove, etc. - parallel processing
     (append
      (base:services)
      (list
       (set-xorg-configuration
        (xorg-configuration
         (keyboard-layout keyboard-layout)))

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
       #;disable-suspend-srvc)










      ;; %desktop-services is the default list of services we are appending to.
      (modify-services %desktop-services
        (gdm-service-type
         config => (gdm-configuration
                    (inherit config)
                    (auto-suspend? #f)
;;; See the Warning above in the xvnc-configuration
                    #;(xdmcp? #t))))))












;;; See
;;; https://guix.gnu.org/manual/en/html_node/Bootloader-Configuration.html
;;; https://www.gnu.org/software/grub/manual/grub/html_node/Invoking-grub_002dinstall.html#Invoking-grub_002dinstall
;;; https://github.com/babariviere/dotfiles/blob/guix/baba/bootloader/grub.scm
    (bootloader
     (bootloader-configuration
      (bootloader grub-bootloader)
      (targets (list "/dev/sda"))
      ;; keyboard-layout for the GRUB
      (keyboard-layout keyboard-layout)))
















;;; The list of file systems that get "mounted". The unique file system
;;; identifiers there ("UUIDs") can be obtained by running 'blkid' in a
;;; terminal.
    (file-systems
     (cons*



            (file-system
              (mount-point "/")
              (device (uuid "cf11628d-4887-42d2-aef1-635ad5089ce1" 'ext4))
              (type "ext4"))




            %base-file-systems))




    (swap-devices (list
                   (swap-space
                    (target (uuid "a4767437-a9c8-4d57-9755-4fcd2aef73da")))))))

(module-evaluated)

syst-config ;; operating-system (or image) must be returned
