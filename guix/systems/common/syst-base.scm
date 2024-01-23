(define-module (syst-base)
  #:use-module (settings)
  #:use-module (utils)                 ; for partial
  #:use-module (memo)
  #:use-module (cfg packages all)      ; for packages-to-install
  #:use-module (gnu)
  #:use-module (gnu system shadow)     ; for user-group; user-account-shell
  #:use-module (guix)                  ; for package-version
)

(use-service-modules
 cups desktop networking ssh
 xorg     ; for gdm-service-type
 sddm     ; for sddm-service-type
 )

(evaluating-module #t)

(define-public keyb-layout
  (keyboard-layout
   "us,de,sk,fr" "altgr-intl,,qwerty,"
   #:options '("compose:menu,grp:ctrls_toggle")))
(testsymb 'keyb-layout)

(define-public (users-config additional-supplementary-groups)
  "The list of user accounts ('root' is implicit)."
  (cons*
   (user-account
    (name user)
    (comment user-full-name)
    (group "users")
    (home-directory home)
    ;; login shell; see also `packages`
    ;; explicitly define fish / bash:
    ;; (shell (file-append fish "/bin/fish"))
    ;; (shell (file-append bash "/bin/bash"))

    ;; list of group names that this user-account belongs to
    (supplementary-groups
     (append
      (list
       ;; sudo etc.; See polkit-wheel-service for administrative tasks
       ;; for non-root users
       "wheel"

       ;; network devices; WiFi network connections done by this user
       ;; are not propagated to other users. IOW every user must know
       ;; and type-in the WIFI passwords by him or herself.
       "netdev"

       "audio"  ;; sound card

       ;; "kvm"
       ;; "tty"
       ;; "input"
       ;; "docker"
       ;; "realtime"  #| Enable realtime scheduling |#
       )
      additional-supplementary-groups)))

   ;; Example of an ordinary, non-privileged user, without root
   ;; permissions and access to home-directories of other users
   ;; (user-account
   ;;  (name "jimb")
   ;;  (comment "Jim Beam")
   ;;  (group "users")
   ;;  (password (crypt "password" "salt")) ;; SHA-512-hashed
   ;;  (home-directory "/home/jimb"))
   %base-user-accounts))
(testsymb-trace 'users-config)

(define-public services
  (list
   ;; ntp-service-type for system clock sync is in the
   ;; %desktop-services by default

   ;; To configure OpenSSH, pass an 'openssh-configuration'
   ;; record as a second argument to 'service' below.
   (service openssh-service-type)

   (service xfce-desktop-service-type)))
(testsymb-trace 'services)

(define-public syst-config
  (operating-system
    (locale "en_US.utf8")
    (timezone "Europe/Berlin")
     ;; keyboard-layout for the console
    (keyboard-layout keyb-layout)
    (host-name "dummy")
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets (list "/boot/efi"))
                 (keyboard-layout keyboard-layout)))
    (file-systems %base-file-systems)))
(testsymb 'syst-config)

(module-evaluated #t)
