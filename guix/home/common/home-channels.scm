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
  #:use-module (bost common utils)
  #:use-module (dotf memo)
  )

(define m (module-name-for-logging))
(evaluating-module)

(define* (home-channels-edge-ecke #:key
                                  bost-commit
                                  games-commit
                                  guix-ai-cloud
                                  guix-android-commit
                                  guix-past-commit
                                  guix-science-commit
                                  guixrus-commit
                                  hask-clj-commit
                                  (use-local-checkout #f)
                                  )
  (list
   ;; dwl window manager for Wayland with dynamic configuration in Guile.
   ;; dwl-guile is a fork of the dwl Wayland Compositor (which is a
   ;; port of dwm - dynamic window manager for X).
   ;; (channel-home-service-dwl-guile)

   ;; When firefox substitutes are not available in the nonguix channel. Fetch
   ;; them from the guix-sciene
   ;; (channel-guix-science #:commit guix-science-commit)

   ;; (channel-guix-android #:commit guix-android-commit)

   ;; whereiseveryone
   ;; (channel-guixrus #:commit guixrus-commit #:use-local-checkout use-local-checkout)

   ;; (channel-hask-clj #:commit hask-clj-commit #:use-local-checkout use-local-checkout)

   ;; For factorio, pulls-in nonguix guix-past
   ;; (channel-games #:commit games-commit #:use-local-checkout use-local-checkout)

   ;; The `guix-past' channel is not needed directly, however it is required by
   ;; the `games' channel, which, without this pinning would pull from the
   ;; latest channel version
   ;; (channel-guix-past #:commit guix-past-commit)

   ;; (channel-home-service-dwl-guile)
   ;; (channel-flat)
   ;; (channel-rde)

   ;; pulls-in: guix nonguix guix-rust-past-crates
   (channel-bost #:commit bost-commit #:use-local-checkout use-local-checkout)

   ;; pulls-in: nonguix
   (channel-guix-ai-cloud #:commit guix-ai-cloud #:use-local-checkout use-local-checkout)

   ))

(def* (home-channels #:key
                     bost-commit
                     games-commit
                     guix-ai-cloud
                     guix-android-commit
                     guix-commit
                     guix-past-commit
                     guix-science-commit
                     guixrus-commit
                     hask-clj-commit
                     nonguix-commit

                     (use-local-checkout #f)
                     #:allow-other-keys
                     )
  ((comp
    (lambda (lst)
      (if (or (host-edge?) (host-ecke?) (host-geek?))
          (append
           (list (channel-nonguix #:commit nonguix-commit
                                  #:use-local-checkout use-local-checkout))
           (home-channels-edge-ecke
            #:bost-commit          bost-commit
            #:guix-ai-cloud        guix-ai-cloud
            #:games-commit         games-commit
            #:guix-android-commit  guix-android-commit
            #:guix-past-commit     guix-past-commit
            #:guix-science-commit  guix-science-commit
            #:guixrus-commit       guixrus-commit
            #:hask-clj-commit      hask-clj-commit

            #:use-local-checkout   use-local-checkout
            ) lst)
          lst)))
   (list (channel-guix #:commit guix-commit))))

(module-evaluated)

(home-channels
 ;; 15 July 2026 17:16:06
 ;; #:nonguix-commit "3b66965566fe8c96edb5a41fd39a9e5a90ad9b61"
 ;; #:bost-commit    "5e19cf4f4b95dffdb7cd12ac4be5be99675f2bd4"
 ;; #:guix-commit    "b1c803d57f4acd95bd802dd6d5e65db9bdc5b0c0"

 ;; 23 July 2026 23:06:31
 ;; #:nonguix-commit "3b66965566fe8c96edb5a41fd39a9e5a90ad9b61"
 ;; #:bost-commit    "81f497315dffb3e62d9c757171084d9b45f1f971"
 ;; #:guix-commit    "65272f778a1aa54f3836c8640aded44267af3241"

 ;; 29 juillet 2026 16:23:46
 ;; #:nonguix-commit "73baab37361b3a81f326aa3fdec78840f5acc577"
 ;; #:bost-commit    "35282eedb5d4f59f09273d0c44e69c9918eb735a"
 ;; #:guix-commit    "dcddca760b8252474498e549b6e6ef34fc393f21"

 ;; 4 août 2026 18:11:58
 ;; #:nonguix-commit "7b7b2c47f9c205ad89ddf54293e7756e797f8980"
 ;; #:bost-commit    "82b0707493422da2c9eab085e2df6bc1ce5baee9"
 ;; #:guix-commit    "d3acc7d021c51e292fe7572d6eef3d2d8d86b1c2"

 ;; 6 août 2026 10:32:05
 ;; #:nonguix-commit "7b7b2c47f9c205ad89ddf54293e7756e797f8980"
 ;; #:bost-commit    "7131259e2c05a04d9e95f1ecbc61d1dcc68677bc"
 ;; #:guix-commit    "e52280b9b64fdf02a4a6cd738896f3a10fc51470"

 ;; 7 août 2026 22:55:18
 ;; #:nonguix-commit "653504e6551198c9b2b998c143d7cf2675b22547"
 ;; #:bost-commit    "163e1d1c9602b602e0ff676f8ea567d2b6a98ba0"
 ;; #:guix-commit    "fea672ffdae11b050fc5cb986ff2ad86f54ed1d1"

 ;; 9 August 2026 21:47:59
 ;; #:nonguix-commit "653504e6551198c9b2b998c143d7cf2675b22547"
 ;; #:bost-commit    "c5c4c82e1ad98d98c46720d19dbf4216dda759e5"
 ;; #:guix-commit    "b9a35ee6b53438d53d66cbe3e82f26bb22a44b5b"

 ;; 10 août 2026 14:25:52
 ;; #:nonguix-commit "653504e6551198c9b2b998c143d7cf2675b22547"
 ;; #:bost-commit    "ceb0df364a22f0d2e249316a5736771b048ca91b"
 ;; #:guix-commit    "714b87a5955f590b79c4f54c071f3f54b66c3c50"

 ;; 10 August 2026 15:49:23
 ;; #:nonguix-commit "653504e6551198c9b2b998c143d7cf2675b22547"
 ;; #:bost-commit    "d70b6844d0c1a41be8be5addc61ff435a22c556e"
 ;; #:guix-commit    "bdc339838a450b64413dc7a4d9fa78cdb30062f7"

 ;; 10 août 2026 20:27:21
 ;; #:nonguix-commit "653504e6551198c9b2b998c143d7cf2675b22547"
 ;; #:bost-commit    "b2c3b690122e0afec40ade444611df2f515758ca"
 ;; #:guix-commit    "bdc339838a450b64413dc7a4d9fa78cdb30062f7"

 ;; 11 août 2026 13:39:21
 ;; #:nonguix-commit       "653504e6551198c9b2b998c143d7cf2675b22547"
 ;; #:bost-commit          "b2c3b690122e0afec40ade444611df2f515758ca"
 ;; #:guix-ai-cloud-commit "05ca2453b4650f7a82b28234f9e512a4ce80d4a4"
 ;; #:guix-commit          "f0332077b948a72f79c16b0838c3c7cdddaed631"

 ;; 11 August 2026 14:57:45
 #:nonguix-commit       "653504e6551198c9b2b998c143d7cf2675b22547"
 #:bost-commit          "0ed162e0aa261636a0afb747db95dd888a27860d"
 #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 #:guix-commit          "c98ec501cce5c4776602ae7cb90b0ba5962ee895"

 #:use-local-checkout #f)

;; It makes no sense to add generation number to the comment. Generation numbers
;; are different on each computer
