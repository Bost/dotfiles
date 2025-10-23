;; `guix pull` places a union of this file with %default-channels to
;; `~/.config/guix/current/manifest`. See
;; guile -c '(use-modules (guix channels)) (format #t "~a\n" %default-channels)'

;; This module is loaded via
;;     --load-path=/home/bost/dev/dotfiles/guix/home/common
;; so the module-name is not (home common home-channels)
(define-module (home-channels)
  #:use-module (gnu services)           ; simple-service
  #:use-module (gnu home services guix) ; home-channels-service-type

  #:use-module (config channels channel-defs)
  #:use-module (utils)
  #:use-module (memo)
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
             )
            ;; (channel-guix-past) ;; pulled-in via channel-games; not needed directly.
            (channel-guixrus ;; whereiseveryone
             ;; 16 sept. 2025 12:33:23
             ;; 18 sept. 2025 10:11:24
             ;; #:commit "83321ce5d23109f143413490aa5acf04361c8076"
             )
            (channel-hask-clj
             ;; 16 sept. 2025 12:33:23
             ;; #:commit "a8b30a606f91caabec3cc8dc4b1255a69836554e"

             )
            ;; (channel-games)
            ;; (channel-home-service-dwl-guile)
            ;; (channel-flat)
            ;; (channel-rde)
            (channel-bost
             ;; #:commit "6ddbd879a9d6e61cbc58721d00a7b1f380bda57a"

             ;; 04 sept. 2025 15:21:12
             ;; #:commit "5c7b61177d0c71c628687c6e0aa9bf29bbb2f836"

             ;; 16 sept. 2025 12:33:23
             ;; #:commit "8eacfc4fece97e9fbff4ecaf7bb3cb181b0c12ca"

             ;; 18 sept. 2025 10:11:24
             ;; #:commit "96d8f9cbc39780a90935d0f49edb372b77702875"
             )
            )
           lst)
          lst)))
   (common-channels
    ;; Aug 31 2025 14:48:46
    ;; #:nonguix-commit "60ffd0353e70d5e371c4bfe2201c9d08c1c05e18"
    ;; #:guix-commit "d431f4620a4c077383e1168f932e86c99ae33834"

    ;; 03 sept. 2025 14:06:29
    ;; #:nonguix-commit "477f283914ca771a8622e16b73d845b87c63335d"
    ;; #:guix-commit "b377ec079d9ffe8f0f372c43735ad012ea889b6f"

    ;; 05 sept. 2025 15:53:28
    ;; #:nonguix-commit "477f283914ca771a8622e16b73d845b87c63335d"
    ;; #:guix-commit "6bffc03be9dcf4711b5d1e9c95aba340403b35df"

    ;; 11 sept. 2025 16:14:49
    ;; #:guix-commit "fe2ed12e66097ee2befc55d7ae88e2a7c19f9e72"
    ;; #:nonguix-commit "d096df03564783372b315fe6c179655c2c337d5a"

    ;; 16 sept. 2025 12:33:23
    ;; #:guix-commit "de41613d273106b88eeed4af72c23a57cfd18442"
    ;; #:nonguix-commit "df4e6ed9fe917f004357d931e210e328e348bb38"

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
    #:guix-commit "3c96d9fb1e1ee8fe4efd44ba6e8f3886b43668be"
    #:nonguix-commit "25d7a8091c2c9678a8694f073d846a7001165169"

    )))
(testsymb 'home-channels)

(home-channels)
