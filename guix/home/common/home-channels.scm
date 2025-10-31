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
      (if (or (is-system-edge) (is-system-ecke))
          (append
           (list
            (channel-guix-android
             ;; 16 sept. 2025 13:50 (roughly)
             ;; 18 sept. 2025 10:11:24
             ;; #:commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"

             ;; 07 oct. 2025 15:33:59
             ;; #:commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"

             ;; 23 oct. 2025 23:06:15
             ;; #:commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
             )
            ;; (channel-guix-past) ;; pulled-in via channel-games; not needed directly.
            (channel-guixrus ;; whereiseveryone
             ;; 16 sept. 2025 12:33:23
             ;; 18 sept. 2025 10:11:24
             ;; #:commit "83321ce5d23109f143413490aa5acf04361c8076"

             ;; 07 oct. 2025 15:33:59
             ;; #:commit "83321ce5d23109f143413490aa5acf04361c8076"

             ;; 23 oct. 2025 23:06:15
             ;; #:commit "40f83be862e31832f4571b826ff6a5f5372e905c"
             )
            (channel-hask-clj
             ;; 16 sept. 2025 12:33:23
             ;; #:commit "a8b30a606f91caabec3cc8dc4b1255a69836554e"

             ;; 23 oct. 2025 23:06:15
             ;; #:commit "a8b30a606f91caabec3cc8dc4b1255a69836554e"
             )
            ;; (channel-games)
            ;; (channel-home-service-dwl-guile)
            ;; (channel-flat)
            ;; (channel-rde)
            (channel-bost
             ;; 16 sept. 2025 12:33:23
             ;; #:commit "8eacfc4fece97e9fbff4ecaf7bb3cb181b0c12ca"

             ;; 18 sept. 2025 10:11:24
             ;; #:commit "96d8f9cbc39780a90935d0f49edb372b77702875"

             ;; 07 oct. 2025 15:33:59
             ;; #:commit "ead5b77819456c577d162e047f8ec6eed9252520"

             ;; 23 oct. 2025 23:06:15
             ;; #:commit "f63e660d12ca11a99052859ebce08d39404b1001"
             ))
           lst)
          lst)))
   (common-channels
    ;; 18 sept. 2025 10:11:24
    ;; #:guix-commit "f62206ecd8605538b1fb18d038d3db33707a8535"
    ;; #:nonguix-commit "df4e6ed9fe917f004357d931e210e328e348bb38"

    ;; 20 sept. 2025 17:50:37
    ;; #:guix-commit "f5b3b375e5d8cae24b02d00f213d09659e60358d"
    ;; #:nonguix-commit "de297a2a28577651cbe27ba58f8b9ea8912392b0"

    ;; 24 2025 22:07:31
    ;; #:guix-commit "aabe0d2c30a2c8143ff70e3f5be1189ff6d494cd"
    ;; #:nonguix-commit "25d7a8091c2c9678a8694f073d846a7001165169"

    ;; 25 sept. 2025 18:59:29
    ;; #:guix-commit "ace78713a9ee7abfdd19e2f009c3fb606b797160"
    ;; #:nonguix-commit "25d7a8091c2c9678a8694f073d846a7001165169"

    ;; 26 sept. 2025 13:03:02
    ;; librewolf not present on substitutes yet
    ;; #:guix-commit "3c96d9fb1e1ee8fe4efd44ba6e8f3886b43668be"
    ;; #:nonguix-commit "25d7a8091c2c9678a8694f073d846a7001165169"

    ;; 07 oct. 2025 15:33:59
    ;; #:guix-commit "11ab410217fa094f040128917e90ea89ee586ffc"
    ;; #:nonguix-commit "a5d216cd7c2d67eb95e58871bb805f22c160c57b"

    ;; 23 oct. 2025 23:06:15
    #:guix-commit "a38e1990eaded3f35c6898aa8fe817a1601702d7"
    #:nonguix-commit "a345ef84fbdf3b2491acb2c2b6665a4eb97bd4aa"
    )))
(testsymb 'home-channels)

(home-channels)
