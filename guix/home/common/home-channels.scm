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

(define m (module-name-for-logging))
(evaluating-module)

(define-public (home-channels-services)
  (list
   ;; (simple-service 'guixrus-service  home-channels-service-type (list (channel-guixrus)))
   (simple-service 'hask-clj-service home-channels-service-type (list (channel-hask-clj)))
   (simple-service 'bost-service     home-channels-service-type (list (channel-bost))))
  )

(define* (home-channels-edge-ecke #:key
                                  guix-science-commit
                                  guix-past-commit
                                  guix-android-commit
                                  guixrus-commit
                                  hask-clj-commit
                                  games-commit
                                  bost-commit
                                  guix-guake-commit
                                  )
  (list
   ;; dwl window manager for Wayland with dynamic configuration in Guile.
   ;; dwl-guile is a fork of the dwl Wayland Compositor (which is a
   ;; port of dwm - dynamic window manager for X).
   ;; (channel-home-service-dwl-guile)

   ;; When firefox substitutes are not available in the nonguix channel. Fetch
   ;; them from the guix-sciene
   (channel-guix-science #:commit guix-science-commit)

   (channel-guix-android #:commit guix-android-commit)

   ;; The `guix-past' channel is not needed directly, however it is required by
   ;; the `games' channel, which, without this pinning would pull from the
   ;; latest channel version
   (channel-guix-past #:commit guix-past-commit)

   ;; whereiseveryone
   ;; (channel-guixrus #:commit guixrus-commit)

   ;; (channel-hask-clj #:commit hask-clj-commit)
   (channel-hask-clj #:commit hask-clj-commit)

   ;; For factorio, pulls-in nonguix guix-past
   (channel-games #:commit games-commit)

   ;; (channel-home-service-dwl-guile)
   ;; (channel-flat)
   ;; (channel-rde)

   ;; pulls-in: guix nonguix guix-rust-past-crates
   (channel-bost #:commit bost-commit)

   ;; guake
   (channel-guix-guake #:commit guix-guake-commit)
   ))

(def* (home-channels #:key
                     guix-science-commit
                     guix-android-commit
                     ;; guixrus-commit
                     hask-clj-commit
                     games-commit
                     bost-commit
                     guix-guake-commit
                     guix-commit
                     nonguix-commit
                     guix-past-commit
                     #:allow-other-keys
                     )
  ((comp
    (lambda (lst)
      (if (or (host-edge?) (host-ecke?) (host-geek?))
          (append
           (list (channel-nonguix #:commit nonguix-commit))
           (home-channels-edge-ecke
            #:guix-science-commit  guix-science-commit
            #:guix-past-commit     guix-past-commit
            #:guix-android-commit  guix-android-commit
            ;; #:guixrus-commit       guixrus-commit
            #:hask-clj-commit      hask-clj-commit
            #:games-commit         games-commit
            #:bost-commit          bost-commit
            #:guix-guake-commit    guix-guake-commit
            ) lst)
          lst)))
   (list (channel-guix #:commit guix-commit))))

(module-evaluated)

(home-channels
 ;; 16 avril 2026 22:53:10
 ;; #:bost-commit          "ecf80aec358b183e079801cd24d6780c5e814f29"
 ;; #:guix-guake-commit    "77c000981152b7295fcf7654aa70a901817005c3"
 ;; #:games-commit         "348775cb228def51bba4f4502e68a078f492b72f"
 ;; #:guix-android-commit  "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-commit          "a375202ad3443e4c3fb492160e32aa6abcc965bd"
 ;; #:guix-past-commit     "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 ;; #:guix-science-commit  "937ad09462d85797a61001ab2c0d664320420b92"
 ;; #:hask-clj-commit      "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:nonguix-commit       "816b5c6f45ed56cd81e2a6338ed5b710e4e66e98"

 ;; 20 avril 2026 12:01:50
 ;; #:bost-commit          "ecf80aec358b183e079801cd24d6780c5e814f29"
 ;; #:guix-guake-commit    "11886ffadc50b8f53403a90812aa0e1a0b139782"
 ;; #:games-commit         "348775cb228def51bba4f4502e68a078f492b72f"
 ;; #:guix-android-commit  "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-commit          "a375202ad3443e4c3fb492160e32aa6abcc965bd"
 ;; #:guix-past-commit     "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 ;; #:guix-science-commit  "937ad09462d85797a61001ab2c0d664320420b92"
 ;; #:hask-clj-commit      "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:nonguix-commit       "816b5c6f45ed56cd81e2a6338ed5b710e4e66e98"

 ;; 22 avril 2026 09:31:03
 ;; #:bost-commit          "ecf80aec358b183e079801cd24d6780c5e814f29"
 ;; #:guix-guake-commit    "952907231b730209eb0fee9499e42da90bf76cf6"
 ;; #:games-commit         "348775cb228def51bba4f4502e68a078f492b72f"
 ;; #:guix-android-commit  "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-commit          "a375202ad3443e4c3fb492160e32aa6abcc965bd"
 ;; #:guix-past-commit     "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 ;; #:guix-science-commit  "937ad09462d85797a61001ab2c0d664320420b92"
 ;; #:hask-clj-commit      "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:nonguix-commit       "816b5c6f45ed56cd81e2a6338ed5b710e4e66e98"

 ;; 30 avril 2026 11:54:11
 ;; #:nonguix-commit      "a3f4e7bff779da4593a2922516064a8edaafa3e6"
 ;; #:guix-science-commit "8ce03731a8eb84afec21953d5706aa72199f6649"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-past-commit    "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:bost-commit         "ecf80aec358b183e079801cd24d6780c5e814f29"
 ;; #:guix-guake-commit   "450bdda20c443d4493fad677e0ccf9861aadf977"
 ;; #:guix-commit         "2dde6fc80f96cd8b1edef8f61637cc2adeb8919f"

 ;; 01 mai 2026 02:00:33
 ;; #:nonguix-commit      "a3f4e7bff779da4593a2922516064a8edaafa3e6"
 ;; #:guix-science-commit "8ce03731a8eb84afec21953d5706aa72199f6649"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-past-commit    "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:bost-commit         "ecf80aec358b183e079801cd24d6780c5e814f29"
 ;; #:guix-guake-commit   "450bdda20c443d4493fad677e0ccf9861aadf977"
 ;; #:guix-commit         "d3cc0386663ebc122edc051a4bad349097016190"

 ;; 06 mai 2026 11:16:53
 #:nonguix-commit      "5f2630e69fbbe9e79c350a67545f0fef7e93e223"
 #:guix-science-commit "4f29b99ce090ed2992d5bc6f3af42180201f5058"
 #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 #:guix-past-commit    "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 #:bost-commit         "ecf80aec358b183e079801cd24d6780c5e814f29"
 #:guix-guake-commit   "450bdda20c443d4493fad677e0ccf9861aadf977"
 #:guix-commit         "81934cf7e9263e47f9fd8defa717365592a3473d"
 )

;; It makes no sense to add generation number to the comment. Generation numbers
;; are different on each computer
