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
 ;; 04. mar. 2026 17:26:51
 ;; #:nonguix-commit       "da4e72efef62d48dbc2eb089c36972ff55fe6acd"
 ;; #:guix-android-commit  "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:hask-clj-commit      "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit         "fcf8e459d8a4fc2800d4a1af14f1c3c6c714e419"
 ;; #:bost-commit          "84999be7778fc2f08e09b056a3aa09afbd9d0c2c"
 ;; #:guix-commit          "c07690f0d673d8415066160617455313afa1d544"
 ;; #:guix-past-commit     "bc95e89da6045cec1797673ba3283122a11e6ee7"

 ;; 04 mar. 2026 20:25:53
 ;; #:nonguix-commit       "816b5c6f45ed56cd81e2a6338ed5b710e4e66e98"
 ;; #:guix-android-commit  "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:hask-clj-commit      "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit         "348775cb228def51bba4f4502e68a078f492b72f"
 ;; #:bost-commit          "84999be7778fc2f08e09b056a3aa09afbd9d0c2c"
 ;; #:guix-commit          "a375202ad3443e4c3fb492160e32aa6abcc965bd"
 ;; #:guix-past-commit     "bc95e89da6045cec1797673ba3283122a11e6ee7"

 ;; 06 mar. 2026 17:01:23
 ;; #:nonguix-commit       "816b5c6f45ed56cd81e2a6338ed5b710e4e66e98"
 ;; #:guix-android-commit  "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:hask-clj-commit      "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit         "348775cb228def51bba4f4502e68a078f492b72f"
 ;; #:bost-commit          "70d2459d1123e5b5a0d1b12979c216e652ef96c6"
 ;; #:guix-commit          "a375202ad3443e4c3fb492160e32aa6abcc965bd"
 ;; #:guix-past-commit     "f5d1cdc58ea810c9d1f1858289b01e9fdc3e4b8f"

 ;; 14 mar. 2026 22:41:34
 ;; #:games-commit         "348775cb228def51bba4f4502e68a078f492b72f"
 ;; #:bost-commit          "9d4396f30a55558b48655a25bc6d9915ea4e13b0"
 ;; #:guix-android-commit  "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-commit          "a375202ad3443e4c3fb492160e32aa6abcc965bd"
 ;; #:guix-past-commit     "f5d1cdc58ea810c9d1f1858289b01e9fdc3e4b8f"
 ;; #:hask-clj-commit      "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:nonguix-commit       "7d14e819fc7b2681240023b9e4c001fc3f5cba04"

 ;; 27 mar. 2026 15:51:03
 ;; #:bost-commit         "4bc752e30201ea9415502909dc3a79749bf4e8a1"
 ;; #:games-commit        "348775cb228def51bba4f4502e68a078f492b72f"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-commit         "29cc182c3571df423ec298044f02f07106cb91d9"
 ;; #:guix-past-commit    "d0e01528abcf07a40981303b3e4a54edd7ffd501"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:nonguix-commit      "b8648bf188df213455983e7eaab9f6c6101ca0c2"

 ;; 12 apr. 2026 13:30:56
 ;; #:bost-commit          "29e431dd2d92175ac54a4ee4e93bacd25a9f5a63"
 ;; #:games-commit         "348775cb228def51bba4f4502e68a078f492b72f"
 ;; #:guix-android-commit  "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-commit          "f1cacebca6d7f9afc7fc81fb18ef6655b12d7f3c"
 ;; #:guix-past-commit     "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 ;; #:guix-science-commit  "937ad09462d85797a61001ab2c0d664320420b92"
 ;; #:hask-clj-commit      "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:nonguix-commit       "c4541fdb0b472664dafe5d7b1ec2e51e4ef7b772"

 ;; 12 apr. 2026 14:14:43
 ;; #:bost-commit          "84999be7778fc2f08e09b056a3aa09afbd9d0c2c"
 ;; #:games-commit         "348775cb228def51bba4f4502e68a078f492b72f"
 ;; #:guix-android-commit  "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-commit          "a375202ad3443e4c3fb492160e32aa6abcc965bd"
 ;; #:guix-past-commit     "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 ;; #:guix-science-commit  "937ad09462d85797a61001ab2c0d664320420b92"
 ;; #:hask-clj-commit      "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:nonguix-commit       "816b5c6f45ed56cd81e2a6338ed5b710e4e66e98"

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
 #:bost-commit          "ecf80aec358b183e079801cd24d6780c5e814f29"
 #:guix-guake-commit    "952907231b730209eb0fee9499e42da90bf76cf6"
 #:games-commit         "348775cb228def51bba4f4502e68a078f492b72f"
 #:guix-android-commit  "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 #:guix-commit          "a375202ad3443e4c3fb492160e32aa6abcc965bd"
 #:guix-past-commit     "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 #:guix-science-commit  "937ad09462d85797a61001ab2c0d664320420b92"
 #:hask-clj-commit      "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 #:nonguix-commit       "816b5c6f45ed56cd81e2a6338ed5b710e4e66e98"
 )

;; It makes no sense to add generation number to the comment. Generation numbers
;; are different on each computer
