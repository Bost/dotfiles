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
                                  bstx-commit
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
   (channel-bstx #:commit bstx-commit #:use-local-checkout use-local-checkout)

   ;; pulls-in: nonguix
   (channel-guix-ai-cloud #:commit guix-ai-cloud #:use-local-checkout use-local-checkout)

   ))

(def* (home-channels #:key
                     bstx-commit
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
            #:bstx-commit          bstx-commit
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
 ;; 1 septembre 2026 16:40:43
 ;; #:nonguix-commit       "accdba77be815bbe00868145dd66d68056eecf7a"
 ;; #:bstx-commit          "c43512e5959601efb7cc2584dfae852028653f5b"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "b8422bc92c56c5c7f22a9378b5ac7695bc487fb4"

 ;; 5 septembre 2026 11:27:01
 ;; #:nonguix-commit       "accdba77be815bbe00868145dd66d68056eecf7a"
 ;; #:bstx-commit          "388a77283a2b74cf7e1b4826be60473b40ebc114"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "99cd3f04169d0eef03b5fc7e3a9e2fec6542c3aa"

 ;; 7 September 2026 11:40:50
 ;; #:nonguix-commit       "accdba77be815bbe00868145dd66d68056eecf7a"
 ;; #:bstx-commit          "49335107503e4a0b0755b135cbedbd233f77a16b"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "d759a1922126909b6097e245631669cf0b368b57"

 ;; 8 septembre 2026 20:59:13
 ;; #:nonguix-commit       "accdba77be815bbe00868145dd66d68056eecf7a"
 ;; #:bstx-commit          "49335107503e4a0b0755b135cbedbd233f77a16b"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "ab3cc11100a7eeff65b9a36d849d0fb96dda4eb9"

 ;; 18 septembre 2026 13:03:29
 #:nonguix-commit       "f9171dd0d0a58d63c0811d61e51493a3fa4ae4f3"
 #:bstx-commit          "49335107503e4a0b0755b135cbedbd233f77a16b"
 #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 #:guix-commit          "0daef659a23220fa76a62dcbda7057cb649f415c"

 #:use-local-checkout #f)

;; It makes no sense to add generation number to the comment. Generation numbers
;; are different on each computer
