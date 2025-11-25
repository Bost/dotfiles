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

(define (home-channels)
  "Channels needed for the Guix-home configuration"
  ((comp
    (lambda (lst)
      (if (or (host-edge?) (host-ecke?))
          (append
           (list
            (channel-guix-android
             ;; 07 oct. 2025 15:33:59
             ;; 23 oct. 2025 23:06:15
             ;; 04 nov. 2025 13:20:55
             ;; 05 nov. 2025 12:48:06
             ;; 14 nov. 2025 14:28:09
             ;; 25 nov. 2025 16:03:05
             #:commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
             )
            ;; (channel-guix-past) ;; pulled-in via channel-games; not needed directly.
            (channel-guixrus ;; whereiseveryone
             ;; 07 oct. 2025 15:33:59
             ;; #:commit "83321ce5d23109f143413490aa5acf04361c8076"

             ;; 23 oct. 2025 23:06:15
             ;; 04 nov. 2025 13:20:55
             ;; 05 nov. 2025 12:48:06
             ;; #:commit "40f83be862e31832f4571b826ff6a5f5372e905c"

             ;; 14 nov. 2025 14:28:09
             ;; #:commit "3df88c90cc0796631d876bb0b87bd4eb881b4286"

             ;; 25 nov. 2025 16:03:05 - from file:///home/bost/dev/guixrus
             #:commit "0a070f3655c32e1aa429d3849dacfaad4fb5b2fa"
             )
            (channel-hask-clj
             ;; 23 oct. 2025 23:06:15
             ;; 04 nov. 2025 13:20:55
             ;; 05 nov. 2025 12:48:06
             ;; 14 nov. 2025 14:28:09
             ;; 25 nov. 2025 16:03:05
             ;; #:commit "a8b30a606f91caabec3cc8dc4b1255a69836554e"
             )
            ;; (channel-games)
            ;; (channel-home-service-dwl-guile)
            ;; (channel-flat)
            ;; (channel-rde)
            (channel-bost
             ;; 07 oct. 2025 15:33:59
             ;; #:commit "ead5b77819456c577d162e047f8ec6eed9252520"

             ;; 23 oct. 2025 23:06:15
             ;; #:commit "f63e660d12ca11a99052859ebce08d39404b1001"

             ;; 04 nov. 2025 13:20:55
             ;; 05 nov. 2025 12:48:06
             ;; 14 nov. 2025 14:28:09
             ;; #:commit "c8ca36f01a616f17eeb6a581ac9de76b57cd9af3"

             ;; 25 nov. 2025 16:03:05
             #:commit "65b4c76a92577493a64a651e20e3c3b4120c9310"
             ))
           lst)
          lst)))
   (common-channels
    ;; 07 oct. 2025 15:33:59
    ;; #:guix-commit "11ab410217fa094f040128917e90ea89ee586ffc"
    ;; #:nonguix-commit "a5d216cd7c2d67eb95e58871bb805f22c160c57b"

    ;; 23 oct. 2025 23:06:15
    ;; #:guix-commit "a38e1990eaded3f35c6898aa8fe817a1601702d7"
    ;; #:nonguix-commit "a345ef84fbdf3b2491acb2c2b6665a4eb97bd4aa"

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
    #:guix-commit "4f3d9137a10d977b0f0ed519a31b90a81f6d92b7"
    #:nonguix-commit "0f68c1684169cbef8824fb246dfefa3e6832225b"
    )))
(testsymb 'home-channels)

(home-channels)
