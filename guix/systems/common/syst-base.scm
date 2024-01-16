(define-module (syst-base)
  #:use-module (settings)
  #:use-module (utils)                 ; for partial
  #:use-module (memo)
  #:use-module (cfg packages all)      ; for packages-to-install
  #:use-module (gnu)
  #:use-module (guix)                  ; for package-version
)

(evaluating-module #t)

(define-public keyb-layout
  (keyboard-layout
   "us,de,sk,fr" "altgr-intl,,qwerty,"
   #:options '("compose:menu,grp:ctrls_toggle")))
(testsymb 'keyb-layout)

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
