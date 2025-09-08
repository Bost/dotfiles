(define-module (syst-base)
  #:use-module (settings)
  #:use-module (utils)                 ; for partial
  #:use-module (memo)
  #:use-module (gnu)
  #:use-module (gnu system shadow)     ; user-group user-account-shell
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (guix)                  ; package-version
)

(use-service-modules
 cups desktop networking ssh
 xorg     ; for gdm-service-type
 sddm     ; for sddm-service-type
 security-token ; for pcscd-service-type
 )

(define m (module-name-for-logging))
(evaluating-module)

(define-public (keyb-layout)
  (keyboard-layout
   ;; EurKEY: The European Keyboard Layout https://eurkey.steffen.bruentjen.eu/
   ;; "eu" ""

   ;; ',,' means that "" ie no modifier is used
   "us,de,sk,fr" "altgr-intl,,qwerty,"

   ;; 1. Use compose key, small menu icon (☰) or the text "Menu":
   ;; While holding ☰,  press 'a', release 'a' and then press 'e'):
   ;; Compose → a → e → æ   ; A-E entrelacé, ash
   ;; Compose → s → s → ß   ; scharfes s
   ;; Compose → ^ → e → ê   ; accent circonflexe
   ;; Compose → , → c → ç   ; cedille
   ;;
   ;; 2. Switch layouts by pressing both Ctrl-keys
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

(define-public (services)
  (list

   ;; PC/SC - Personal Computer/Smart Card: specification for smart-card
   ;; integration into computing environments.
   (service pcscd-service-type)

   ;; ntp-service-type for system clock sync is in the
   ;; %desktop-services by default

   ;; Enables `ssh <host>` without requiring a direct login to the <host>.
   ;; (service dhcp-client-service-type)

   ;; To configure OpenSSH, pass an 'openssh-configuration'
   ;; record as a second argument to 'service' below.
   (service
    openssh-service-type
    (openssh-configuration
     ;; (permit-root-login 'prohibit-password)
     (password-authentication? #f) ;; default: #t
     (authorized-keys
      ;; Assuming the id_rsa.pub exists under given path, e.g. it was
      ;; transferred by `ssh-copy-id` at some point in the past.
      `((,user ,(local-file (string-append home "/.ssh/id_rsa.pub")))))))

   (service xfce-desktop-service-type)))
(testsymb-trace 'services)

(define-public (syst-config)
  (operating-system
    (locale "en_US.utf8")
    (timezone "Europe/Berlin")
     ;; keyboard-layout for the console
    (keyboard-layout (keyb-layout))
    (host-name "dummy") ; host-name is mandatory
    (services
     (append
      (list
;;; Enable SysRq (PRT SC / Drucken) "magic keys" by putting 'kernel.sysrq=1' in
;;; /etc/sysctl.conf
;;; Alt+SysRq+f       OOM killer immediately kills something big
;;; Test by simulated pressing Alt+SysRq+h (help).
;;; echo h | sudo tee /proc/sysrq-trigger && sudo dmesg --ctime | tail -n10
       (service sysctl-service-type
                (sysctl-configuration
                 (settings '(("kernel.sysrq" . "1"))))))
      %base-services))
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets (list "/boot/efi"))
                 (keyboard-layout keyboard-layout)))
    (file-systems %base-file-systems)))
(testsymb 'syst-config)

(define-public (syst-config-linux)
  (operating-system
    (inherit (syst-config))
    (kernel linux)

;;; CPU microcode updates are nonfree blobs that apply directly to a processor
;;; to patch its behavior, and are therefore not included in upstream GNU Guix.
;;; However, running the latest microcode is important to avoid nasty CPU bugs
;;; and hardware security vulnerabilities.
    (initrd microcode-initrd)

    (firmware (list linux-firmware))))
(testsymb 'syst-config-linux)

(module-evaluated)
