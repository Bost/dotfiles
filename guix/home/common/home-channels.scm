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
 ;; 27 mai 2026 17:11:48
 ;; #:nonguix-commit      "e1273e751bb4a65cd8f817b871bfde740373d917"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:guix-past-commit    "a0ea643f565d5aaa70e50e6a83865389aa5d80ba"
 ;; #:guix-guake-commit   "0344ac31a3a8b2dc0b3a43bfed63cf80f205b311"
 ;; #:bost-commit         "d54720c8d923dec686049efc6c32e07856ed9139"
 ;; #:guix-commit         "19da15f8713af74f1f7fec0bc3864efa92f43129"

 ;; 28 mai 2026 15:36:11
 ;; #:nonguix-commit      "e1273e751bb4a65cd8f817b871bfde740373d917"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:guix-past-commit    "a0ea643f565d5aaa70e50e6a83865389aa5d80ba"
 ;; #:guix-guake-commit   "0344ac31a3a8b2dc0b3a43bfed63cf80f205b311"
 ;; #:bost-commit         "d54720c8d923dec686049efc6c32e07856ed9139"
 ;; #:guix-commit         "48dda47112f2e427ca17c9ccfa6ce83c026e228d"

 ;; 30 mai 2026 13:50:17
 ;; #:nonguix-commit      "01ccd92a73434029119c0ef1bd8e0f1a7bb71a8e"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:guix-past-commit    "a0ea643f565d5aaa70e50e6a83865389aa5d80ba"
 ;; #:guix-guake-commit   "0344ac31a3a8b2dc0b3a43bfed63cf80f205b311"
 ;; #:bost-commit         "d54720c8d923dec686049efc6c32e07856ed9139"
 ;; #:guix-commit         "33acdbdfb19aff97150c7922ed97e250825f566b"

 ;; 30 mai 2026 15:09:44
 ;; #:nonguix-commit      "3ed7c207c59dde11a97db483cad4c96eae1a10c4"
 ;; #:guix-commit         "c905bdf006e13ac1bf8f2ff375674d7cfa484bac"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:guix-past-commit    "a0ea643f565d5aaa70e50e6a83865389aa5d80ba"
 ;; #:guix-guake-commit   "0344ac31a3a8b2dc0b3a43bfed63cf80f205b311"
 ;; #:bost-commit         "57852aa6743234c335c68f190ee04d2e2be40ff0"

 ;; 30 mai 2026 15:29:25
 ;; #:nonguix-commit      "3ed7c207c59dde11a97db483cad4c96eae1a10c4"
 ;; #:guix-commit         "c905bdf006e13ac1bf8f2ff375674d7cfa484bac"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:guix-past-commit    "a0ea643f565d5aaa70e50e6a83865389aa5d80ba"
 ;; #:guix-guake-commit   "0344ac31a3a8b2dc0b3a43bfed63cf80f205b311"
 ;; #:bost-commit         "8d89c3836fdef57193136b12a39dae52d8204fd8"

 ;; 30 mai 2026 15:29:25
 ;; #:nonguix-commit      "3ed7c207c59dde11a97db483cad4c96eae1a10c4"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:guix-past-commit    "a0ea643f565d5aaa70e50e6a83865389aa5d80ba"
 ;; #:guix-guake-commit   "0344ac31a3a8b2dc0b3a43bfed63cf80f205b311"
 ;; #:bost-commit         "8d89c3836fdef57193136b12a39dae52d8204fd8"
 ;; #:guix-commit         "c905bdf006e13ac1bf8f2ff375674d7cfa484bac"

 ;; 30 mai 2026 17:20:28
 ;; #:nonguix-commit      "3ed7c207c59dde11a97db483cad4c96eae1a10c4"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:guix-past-commit    "a0ea643f565d5aaa70e50e6a83865389aa5d80ba"
 ;; #:guix-guake-commit   "0344ac31a3a8b2dc0b3a43bfed63cf80f205b311"
 ;; #:bost-commit         "500c24c9bbb5e34840ffe378cb602884279f3c47"
 ;; #:guix-commit         "c905bdf006e13ac1bf8f2ff375674d7cfa484bac"

 ;; 06 juin 2026 19:23:09
 ;; #:nonguix-commit    "3ed7c207c59dde11a97db483cad4c96eae1a10c4"
 ;; #:guix-guake-commit "0344ac31a3a8b2dc0b3a43bfed63cf80f205b311"
 ;; #:bost-commit       "308ba7fb5a2e5a82226dd13f749929541ac14709"
 ;; #:guix-commit       "53fabf4eb1f91e670f391d579c31b88ff83a207c"

 ;; 07 juin 2026 20:34:50
 ;; #:nonguix-commit    "3ed7c207c59dde11a97db483cad4c96eae1a10c4"
 ;; #:guix-guake-commit "0344ac31a3a8b2dc0b3a43bfed63cf80f205b311"
 ;; #:bost-commit       "39157094fb13efd41e346ad15ed4cf64ad1938cd"
 ;; #:guix-commit       "53fabf4eb1f91e670f391d579c31b88ff83a207c"

 ;; 07 juin 2026 21:45:01
 ;; #:nonguix-commit    "3ed7c207c59dde11a97db483cad4c96eae1a10c4"
 ;; #:guix-guake-commit "0344ac31a3a8b2dc0b3a43bfed63cf80f205b311"
 ;; #:bost-commit       "9e43934b8e07338cdd1ec4d62c3381d0b30f5169"
 ;; #:guix-commit       "53fabf4eb1f91e670f391d579c31b88ff83a207c"

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
 #:nonguix-commit "d35a2f8f22023426ccf3598fa7079b09bb821e3e"
 #:bost-commit    "62a193df129b5a7a0da6af4f586b2b51d5b10629"
 #:guix-commit    "1ddddf2a1235dc8a320def0c0808a905453315a7"

 #:use-local-checkout #f)

;; It makes no sense to add generation number to the comment. Generation numbers
;; are different on each computer
