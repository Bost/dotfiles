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
   (channel-guix-guake #:commit guix-guake-commit #:use-local-checkout use-local-checkout)

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
 ;; 14 mai 2026 16:51:28
 ;; #:nonguix-commit      "5f2630e69fbbe9e79c350a67545f0fef7e93e223"
 ;; #:guix-science-commit "c0321213ed34a76fc513a0f930d5d072b6785bec"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-past-commit    "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:bost-commit         "067c601412c5d6678e9f225f92017362d763b951"
 ;; #:guix-guake-commit   "450bdda20c443d4493fad677e0ccf9861aadf977"
 ;; #:guix-commit         "a6f6e6c7b3354a2138b7573b352a4fa3bc6a10be"

 ;; 15 mai 2026 16:38:36
 ;; #:nonguix-commit      "5f2630e69fbbe9e79c350a67545f0fef7e93e223"
 ;; #:guix-science-commit "c0321213ed34a76fc513a0f930d5d072b6785bec"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-past-commit    "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:bost-commit         "92e3160b208aa898023c91d4d944d971161b9713"
 ;; #:guix-guake-commit   "450bdda20c443d4493fad677e0ccf9861aadf977"
 ;; #:guix-commit         "a6f6e6c7b3354a2138b7573b352a4fa3bc6a10be"

 ;; 18 mai 2026 15:42:35
 ;; #:nonguix-commit      "5f2630e69fbbe9e79c350a67545f0fef7e93e223"
 ;; #:guix-science-commit "c0321213ed34a76fc513a0f930d5d072b6785bec"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-past-commit    "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:bost-commit         "8a211715352a1862658791f4b3105f4dbf8a7682"
 ;; #:guix-guake-commit   "450bdda20c443d4493fad677e0ccf9861aadf977"
 ;; #:guix-commit         "c7e85bab139ebc9b59ffc0e1f773fb79634ce2ce"

 ;; 18 mai 2026 16:18:37
 ;; #:nonguix-commit      "5f2630e69fbbe9e79c350a67545f0fef7e93e223"
 ;; #:guix-science-commit "24317029245eddca35f64b81280a831a18bc680c"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-past-commit    "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:bost-commit         "8a211715352a1862658791f4b3105f4dbf8a7682"
 ;; #:guix-guake-commit   "450bdda20c443d4493fad677e0ccf9861aadf977"
 ;; #:guix-commit         "928f7ff31a61d6e3f764d6ce32bfe3bb4c9db9e8"

 ;; 19 mai 2026 13:23:10
 ;; #:nonguix-commit      "5f2630e69fbbe9e79c350a67545f0fef7e93e223"
 ;; #:guix-science-commit "3e887c81a68c38aede80d92947b449f9c254602f"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-past-commit    "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:bost-commit         "22bbae9a7045a852563e89657c7517fc779d4188"
 ;; #:guix-guake-commit   "450bdda20c443d4493fad677e0ccf9861aadf977"
 ;; #:guix-commit         "7309f55abc4010037eda80cb5d2afd4548db5b25"

 ;; 19 mai 2026 21:57:40
 ;; #:nonguix-commit      "5f2630e69fbbe9e79c350a67545f0fef7e93e223"
 ;; #:guix-science-commit "3e887c81a68c38aede80d92947b449f9c254602f"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-past-commit    "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:bost-commit         "1c910ece10bdd8afca04a63b943f1001075257f0"
 ;; #:guix-guake-commit   "450bdda20c443d4493fad677e0ccf9861aadf977"
 ;; #:guix-commit         "5f42ab7d46cf22b19844521b2385a7890faf274b"

 ;; 21 mai 2026 12:40:20
 ;; #:nonguix-commit      "5f2630e69fbbe9e79c350a67545f0fef7e93e223"
 ;; #:guix-science-commit "76200c5a049c59eb9f7622cbafc20bafddf3750a"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-past-commit    "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:bost-commit         "3629589bc85d0736ba64494408a45d59fbe439ee"
 ;; #:guix-guake-commit   "450bdda20c443d4493fad677e0ccf9861aadf977"
 ;; #:guix-commit         "1672274e6f2bb20fb72bc8150ce4d0225281ac6b"

 ;; 22 mai 2026 13:29:31
 ;; #:nonguix-commit      "5f2630e69fbbe9e79c350a67545f0fef7e93e223"
 ;; #:guix-science-commit "76200c5a049c59eb9f7622cbafc20bafddf3750a"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-past-commit    "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:bost-commit         "923714836eddc9ffb8ded1f91a9b85078dbad201"
 ;; #:guix-guake-commit   "450bdda20c443d4493fad677e0ccf9861aadf977"
 ;; #:guix-commit         "1672274e6f2bb20fb72bc8150ce4d0225281ac6b"

 ;; 22 mai 2026 13:40:15
 ;; #:nonguix-commit      "0107c1c4849460b567424ba2cdc8942294c7a611"
 ;; #:guix-science-commit "76200c5a049c59eb9f7622cbafc20bafddf3750a"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-past-commit    "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:bost-commit         "977f4539406fa62d4995ce33e1e83098736e9be6"
 ;; #:guix-guake-commit   "450bdda20c443d4493fad677e0ccf9861aadf977"
 ;; #:guix-commit         "e6b49f7d47cb1c38bfeba661a5e5cc6d1d17c4f4"

 ;; May 22 2026 16:22:41
 ;; #:nonguix-commit      "0107c1c4849460b567424ba2cdc8942294c7a611"
 ;; #:guix-science-commit "76200c5a049c59eb9f7622cbafc20bafddf3750a"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:guix-past-commit    "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 ;; #:guix-guake-commit   "450bdda20c443d4493fad677e0ccf9861aadf977"
 ;; #:bost-commit         "80e1dcb489b51ff226976ee3a444a466912d46ad"
 ;; #:guix-commit         "e6b49f7d47cb1c38bfeba661a5e5cc6d1d17c4f4"

 ;; 23 mai 2026 20:02:23
 ;; #:nonguix-commit      "0107c1c4849460b567424ba2cdc8942294c7a611"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:guix-past-commit    "a0ea643f565d5aaa70e50e6a83865389aa5d80ba"
 ;; #:guix-guake-commit   "0344ac31a3a8b2dc0b3a43bfed63cf80f205b311"
 ;; #:bost-commit         "c29b25bd632a955bd355979bd081987a8683fc04"
 ;; #:guix-commit         "0b597e551715ca6cbdbb2ffeacd530624a01d200"

 ;; 26 mai 2026 13:42:55
 ;; #:nonguix-commit      "ffa33d200e1c930e79d00021f03b2f8c31d00b61"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "8c8f3383cda8e269f40cdfcdd020c172cfd62312"
 ;; #:guix-past-commit    "a0ea643f565d5aaa70e50e6a83865389aa5d80ba"
 ;; #:guix-guake-commit   "0344ac31a3a8b2dc0b3a43bfed63cf80f205b311"
 ;; #:bost-commit         "d54720c8d923dec686049efc6c32e07856ed9139"
 ;; #:guix-commit         "8c95d6364a34cfbd60efbd15d85ce8b02ff6a2cd"

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
 #:nonguix-commit    "bf39542ca537fde8839b209ac21d6f3254469b15"
 #:guix-guake-commit "0344ac31a3a8b2dc0b3a43bfed63cf80f205b311"
 #:bost-commit       "b8bec1ed2b041b703c4ade0d9f91e7651b221073"
 #:guix-commit       "d32d199b43a598acc57d8077c35c9f93874ab562"

 #:use-local-checkout #f)

;; It makes no sense to add generation number to the comment. Generation numbers
;; are different on each computer
