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

(define* (home-channels-edge-ecke #:key
                                  guix-science-commit
                                  guix-past-commit
                                  guix-android-commit
                                  guixrus-commit
                                  hask-clj-commit
                                  games-commit
                                  bost-commit
                                  guix-guake-commit
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

   ;; guake
   ;; (channel-guix-guake #:commit guix-guake-commit #:use-local-checkout use-local-checkout)

   ;; pulls-in: guix nonguix guix-rust-past-crates
   (channel-bost #:commit bost-commit #:use-local-checkout use-local-checkout)
   ))

(def* (home-channels #:key
                     guix-science-commit
                     guix-android-commit
                     guixrus-commit
                     hask-clj-commit
                     games-commit
                     bost-commit
                     guix-guake-commit
                     guix-commit
                     nonguix-commit
                     guix-past-commit
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
            #:guix-science-commit  guix-science-commit
            #:guix-past-commit     guix-past-commit
            #:guix-android-commit  guix-android-commit
            #:guixrus-commit       guixrus-commit
            #:hask-clj-commit      hask-clj-commit
            #:games-commit         games-commit
            #:bost-commit          bost-commit
            #:guix-guake-commit    guix-guake-commit
            #:use-local-checkout   use-local-checkout
            ) lst)
          lst)))
   (list (channel-guix #:commit guix-commit))))

(module-evaluated)

(home-channels
 ;; 11 juin 2026 15:00:35
 ;; #:nonguix-commit    "4ae06fb5cb75f2ca6b0f2f384f41677ae28c069a"
 ;; #:guix-guake-commit "0344ac31a3a8b2dc0b3a43bfed63cf80f205b311"
 ;; #:bost-commit       "9e43934b8e07338cdd1ec4d62c3381d0b30f5169"
 ;; #:guix-commit       "e494c2bd3de8087ac19c1fce9effb3128b35091e"

 ;; 13 juin 2026 14:51:56
 ;; #:nonguix-commit    "4ae06fb5cb75f2ca6b0f2f384f41677ae28c069a"
 ;; #:guix-guake-commit "0344ac31a3a8b2dc0b3a43bfed63cf80f205b311"
 ;; #:bost-commit       "b8bec1ed2b041b703c4ade0d9f91e7651b221073"
 ;; #:guix-commit       "32574d8377643af53cbef88d0fb8ed3c8774867d"

 ;; 18 juin 2026 11:58:32
 ;; #:nonguix-commit    "bf39542ca537fde8839b209ac21d6f3254469b15"
 ;; #:guix-guake-commit "0344ac31a3a8b2dc0b3a43bfed63cf80f205b311"
 ;; #:bost-commit       "b8bec1ed2b041b703c4ade0d9f91e7651b221073"
 ;; #:guix-commit       "d32d199b43a598acc57d8077c35c9f93874ab562"

 ;; 22 juin 2026 13:10:38
 ;; #:nonguix-commit    "bf39542ca537fde8839b209ac21d6f3254469b15"
 ;; #:guix-guake-commit "0344ac31a3a8b2dc0b3a43bfed63cf80f205b311"
 ;; #:bost-commit       "a927285a66cd68589b814a08e2222ebc9d4ae73f"
 ;; #:guix-commit       "a24d3b2d8cb9857a677388530a3de6c4ecd8fec2"

 ;; 24 juin 2026 13:36:31
 ;; #:nonguix-commit    "bf39542ca537fde8839b209ac21d6f3254469b15"
 ;; #:guix-guake-commit "0344ac31a3a8b2dc0b3a43bfed63cf80f205b311"
 ;; #:bost-commit       "a927285a66cd68589b814a08e2222ebc9d4ae73f"
 ;; #:guix-commit       "6d7a9269eac93919f050ff0146e261e1b27d1cbe"

 ;; 24 juin 2026 20:51:05
 ;; #:nonguix-commit "bf39542ca537fde8839b209ac21d6f3254469b15"
 ;; #:bost-commit    "2cbab90a44110469ad0a9a14aa092b31bc206c18"
 ;; #:guix-commit    "51069e88bebd89f1b7b28cc8b8086c819fab9cad"

 ;; 24 juin 2026 21:41:41
 ;; #:nonguix-commit "bf39542ca537fde8839b209ac21d6f3254469b15"
 ;; #:bost-commit    "3bc2a4451b62187eb9fad89806351afa2ks77091f"
 ;; #:guix-commit    "2114a3355448d55f5803b63f4043abd3fff86417"

 ;; 26 juin 2026 00:14:56
 ;; #:nonguix-commit "bf39542ca537fde8839b209ac21d6f3254469b15"
 ;; #:bost-commit    "c3147fe68fafaac5e0910cf7fde218a7df64242d"
 ;; #:guix-commit    "2114a3355448d55f5803b63f4043abd3fff86417"

 ;; 26 juin 2026 00:47:18
 ;; #:nonguix-commit "bf39542ca537fde8839b209ac21d6f3254469b15"
 ;; #:bost-commit    "c3147fe68fafaac5e0910cf7fde218a7df64242d"
 ;; #:guix-commit    "e95294e0b5ba697592044ed24120a45d8463c074"

 ;; 27 juin 2026 13:01:58
 ;; #:nonguix-commit "d35a2f8f22023426ccf3598fa7079b09bb821e3e"
 ;; #:bost-commit    "c3147fe68fafaac5e0910cf7fde218a7df64242d"
 ;; #:guix-commit    "ca200940599e89ec8bc9e46a1a9b6091aa1ec47f"

 ;; 30 June 2026 16:24:22
 ;; #:nonguix-commit "d35a2f8f22023426ccf3598fa7079b09bb821e3e"
 ;; #:bost-commit    "62a193df129b5a7a0da6af4f586b2b51d5b10629"
 ;; #:guix-commit    "1ddddf2a1235dc8a320def0c0808a905453315a7"

 ;; 1 July 2026 13:00:39
 ;; #:nonguix-commit "d35a2f8f22023426ccf3598fa7079b09bb821e3e"
 ;; #:bost-commit    "62a193df129b5a7a0da6af4f586b2b51d5b10629"
 ;; #:guix-commit    "38031dd2c8b08bb21cc429f981a2ca843c205bd5"

 ;; 4 juillet 2026 14:12:51
 ;; #:nonguix-commit "4bc86c61d5ab661614b099bfe524f7f5798988b3"
 ;; #:bost-commit    "5e19cf4f4b95dffdb7cd12ac4be5be99675f2bd4"
 ;; #:guix-commit    "a118e78776390b1e56928927d5056cd9426d786e"

 ;; 9 July 2026 11:48:44
 ;; #:nonguix-commit "fe63493aba7ad6107cb938fcd307c400b53a32b1"
 ;; #:bost-commit    "5e19cf4f4b95dffdb7cd12ac4be5be99675f2bd4"
 ;; #:guix-commit    "09a208a590dfc0fc49cd7f325cd2a0fd6c9d5c12"

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

 ;; 4 août 2026 16:20:22
 #:nonguix-commit "7b7b2c47f9c205ad89ddf54293e7756e797f8980"
 #:bost-commit    "55dc35853c3a0429d37fd367a6c6e556b0ef3b5c"
 #:guix-commit    "86813d5779253bb50002d79ab791eeda5a8b4729"

 #:use-local-checkout #f)

;; It makes no sense to add generation number to the comment. Generation numbers
;; are different on each computer
