;; `guix pull` places a union of this file with %default-channels to
;; `~/.config/guix/current/manifest`. See
;; guile -c '(use-modules (guix channels)) (format #t "~a\n" %default-channels)'

;; This module is loaded via
;;     --load-path=/home/bost/dev/dotfiles/guix/home/common
;; so the module-name is not (home common home-channels)
(define-module (home-channels)
  #:use-module (gnu services)           ; simple-service
  #:use-module (gnu home services guix) ; home-channels-service-type

  #:use-module (dotf config channels channel-defs)
  #:use-module (dotf utils)
  #:use-module (dotf memo)
  )

(define-public (home-channels-services)
  (list
   ;; (simple-service 'guixrus-service  home-channels-service-type (list (channel-guixrus)))
   (simple-service 'hask-clj-service home-channels-service-type (list (channel-hask-clj)))
   (simple-service 'bost-service     home-channels-service-type (list (channel-bost))))
  )

(define (home-channels-edge-ecke)
  (list
   ;; dwl window manager for Wayland with dynamic configuration in Guile.
   ;; dwl-guile is a fork of the dwl Wayland Compositor (which is a
   ;; port of dwm - dynamic window manager for X).
   ;; (channel-home-service-dwl-guile)

   (channel-guix-android
    ;; 04 nov. 2025 13:20:55
    ;; 05 nov. 2025 12:48:06
    ;; 14 nov. 2025 14:28:09
    ;; 25 nov. 2025 16:03:05
    ;; 02 déc. 2025 19:12:22
    ;; 10 déc. 2025 21:45:14
    ;; 29 déc. 2025 16:13:48
    ;; 05 jan. 2026 14:03:59
    ;; 12 jan. 2026 12:30:03
    #:commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
    )
   ;; (channel-guix-past) ;; pulled-in via channel-games; not needed directly.
   (channel-guixrus ;; whereiseveryone
    ;; 04 nov. 2025 13:20:55
    ;; 05 nov. 2025 12:48:06
    ;; #:commit "40f83be862e31832f4571b826ff6a5f5372e905c"

    ;; 14 nov. 2025 14:28:09
    ;; #:commit "3df88c90cc0796631d876bb0b87bd4eb881b4286"

    ;; 25 nov. 2025 16:03:05 - from file:///home/bost/dev/guixrus
    ;; 02 déc. 2025 19:12:22
    ;; #:commit "0a070f3655c32e1aa429d3849dacfaad4fb5b2fa"

    ;; 10 déc. 2025 21:45:14 - from file:///home/bost/dev/guixrus
    ;; #:commit "29573922e8c749a4a3b52d0a9fc0b71e13b4a12c"

    ;; 29 déc. 2025 16:13:48 - from file:///home/bost/dev/guixrus
    ;; 05 jan. 2026 14:03:59 - from file:///home/bost/dev/guixrus
    ;; 12 jan. 2026 12:30:03 - from file:///home/bost/dev/guixrus
    #:commit "9f5a0a357ffafbe9b31fcdc3916dfee57baf5859"
    )
   (channel-hask-clj
    ;; 04 nov. 2025 13:20:55
    ;; 05 nov. 2025 12:48:06
    ;; 14 nov. 2025 14:28:09
    ;; 25 nov. 2025 16:03:05
    ;; 02 déc. 2025 19:12:22
    ;; 10 déc. 2025 21:45:14
    ;; 29 déc. 2025 16:13:48
    ;; 05 jan. 2026 14:03:59
    ;; 12 jan. 2026 12:30:03
    ;; #:commit "a8b30a606f91caabec3cc8dc4b1255a69836554e"
    )
   (channel-games ; For factorio; pulls-in nonguix guix-past
    ;; 29 déc. 2025 16:13:48
    ;; 05 jan. 2026 14:03:59
    ;; 12 jan. 2026 12:30:03
    #:commit "acc252d2f7fed939ace5a5a98d7750197696dac3"
    )

   ;; (channel-home-service-dwl-guile)
   ;; (channel-flat)
   ;; (channel-rde)
   (channel-bost ; pulls-in: guix nonguix guix-rust-past-crates
    ;; 04 nov. 2025 13:20:55
    ;; 05 nov. 2025 12:48:06
    ;; 14 nov. 2025 14:28:09
    ;; #:commit "c8ca36f01a616f17eeb6a581ac9de76b57cd9af3"

    ;; 25 nov. 2025 16:03:05
    ;; #:commit "65b4c76a92577493a64a651e20e3c3b4120c9310"

    ;; 02 déc. 2025 19:12:22
    ;; 10 déc. 2025 21:45:14
    ;; 29 déc. 2025 16:13:48
    ;; #:commit "2d6098abb21cbae397d548ced46878bcf07cade9"

    ;; 05 jan. 2026 14:03:59
    ;; #:commit "6b973948644b41e24d777830b22806bd47d6c388"

    ;; 12 jan. 2026 12:30:03
    #:commit "09700d780a7f706c910221d5d1de9378dc56ab12"
    )))

(define (home-channels)
  "Channels needed for the Guix-home configuration"
  ((comp
    (lambda (lst)
      (if (or (host-edge?) (host-ecke?))
          (append (home-channels-edge-ecke) lst)
          lst)))
   (common-channels
    ;; 04 nov. 2025 13:20:55
    ;; #:guix-commit "873fc541a17f3a41d72c6b375754e035686de3fe"
    ;; #:nonguix-commit "74b20a74f9b37944f9532f5b649e962b50068faf"

    ;; 05 nov. 2025 12:48:06
    ;; #:guix-commit "47df71794f7ee9fc09398382feac12a5d39e6ddd"
    ;; #:nonguix-commit "74b20a74f9b37944f9532f5b649e962b50068faf"

    ;; 14 nov. 2025 14:28:09
    ;; #:guix-commit "7e286874b81dc1d4f60153ae80348e493eda35c1"
    ;; #:nonguix-commit "0f68c1684169cbef8824fb246dfefa3e6832225b"

    ;; 25 nov. 2025 16:03:05
    ;; #:guix-commit "4f3d9137a10d977b0f0ed519a31b90a81f6d92b7"
    ;; #:nonguix-commit "0f68c1684169cbef8824fb246dfefa3e6832225b"

    ;; 02 déc. 2025 19:12:22
    ;; #:guix-commit "5f7cc5c2c6497ebaf7039cdb30ceba025602d698"
    ;; #:nonguix-commit "82be0b7adaaaa7a98d47382d7f72dd2e31d8e6d8"

    ;; 10 déc. 2025 21:45:14
    ;; #:guix-commit "ec959892550ecbfcd4be47e8464df953af6529b3"
    ;; #:nonguix-commit "cb35c71a028eb4a7c950c6a9637c0efad413ef35"

    ;; 29 déc. 2025 16:13:48
    ;; #:guix-commit "d88ecb34b4ec6ce43e34def8e54b2f2522706721"
    ;; #:nonguix-commit "ba91bc437dba367f98608658bb4a19fb0880ad24"

    ;; 05 jan. 2026 14:03:59
    ;; #:guix-commit "4018a134736f7530c95ee6330e65e22b94ae0132"
    ;; #:nonguix-commit "523500471fbc28972d750aad6dc1b8379a6632d6"

    ;; 12 jan. 2026 12:30:03
    #:guix-commit "62c28bc6d8c54800b16341d1c93a1d7833fc3328"
    #:nonguix-commit "3ceef556e1ce8e52180b959a942b16000ed4e61c"
    )))
(testsymb 'home-channels)

(home-channels)
