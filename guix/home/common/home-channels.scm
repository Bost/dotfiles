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
                                  guix-ai-cloud-commit
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
   (channel-guix-ai-cloud #:commit guix-ai-cloud-commit #:use-local-checkout use-local-checkout)

   ))

(def* (home-channels #:key
                     bstx-commit
                     games-commit
                     guix-ai-cloud-commit
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
            #:guix-ai-cloud-commit guix-ai-cloud-commit
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
 ;; 24 October 2026 15:27
 ;; #:nonguix-commit       "c241d19ff840dc75556f722b231760d119b6adea"
 ;; #:bstx-commit          "47a468b9a842c5e5ea9f334d7240b317dc22df57"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "973dbc8c00de826f3c38468b09519d30d064eea3"

 ;; 28 October 2026 14:57
 ;; #:nonguix-commit       "c241d19ff840dc75556f722b231760d119b6adea"
 ;; #:bstx-commit          "d18c7b445ac9758da87cec10e32eff1cc8fbac31"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "7c76ac3104534da484372241010dab32a0cf65b2"

 ;; 30 October 2026 15:26
 ;; #:nonguix-commit       "c241d19ff840dc75556f722b231760d119b6adea"
 ;; #:bstx-commit          "d18c7b445ac9758da87cec10e32eff1cc8fbac31"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "3d3ceb703652b069d5d8d6241f430072e95ddf2a"

 ;; 3 November 2026 23:26
 ;; #:nonguix-commit       "63d83b22615e7a97a4fb0c3d9f6419e9a7fc4fe0"
 ;; #:bstx-commit          "d18c7b445ac9758da87cec10e32eff1cc8fbac31"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "b7f6549b5026b5776141482f9e228c49f09b5569"

 ;; 9 November 2026 17:05
 ;; #:nonguix-commit       "775ff3118af3fc7fb77e2a50da970045f0680a0d"
 ;; #:bstx-commit          "d18c7b445ac9758da87cec10e32eff1cc8fbac31"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "879a07a3f2c6a2198a98367ec1d1e8bc8d37db48"

 ;; 17 November 2026 14:28
 ;; #:nonguix-commit       "57b156aa003e2b6eacf979cac377e4b585bf3a19"
 ;; #:bstx-commit          "d18c7b445ac9758da87cec10e32eff1cc8fbac31"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "634839060dcfdab5819442225a154958b1f3df22"

 ;; 19 November 2026 01:59
 ;; #:nonguix-commit       "57b156aa003e2b6eacf979cac377e4b585bf3a19"
 ;; #:bstx-commit          "16a4eb9833d5d21fe4a271f481c7bc1cd74da7a6"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "f012abae216218963a5b1e5275575b4b50847b23"

 ;; 20 November 2026 15:27
 ;; #:nonguix-commit       "57b156aa003e2b6eacf979cac377e4b585bf3a19"
 ;; #:bstx-commit          "16a4eb9833d5d21fe4a271f481c7bc1cd74da7a6"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "6ce797d03f395311b559697af381d590942cc52a"

 ;; 20 November 2026 16:37
 #:nonguix-commit       "57b156aa003e2b6eacf979cac377e4b585bf3a19"
 #:bstx-commit          "22721bcd98676dd77250c2319f67ca71d7df02ce"
 #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 #:guix-commit          "0a08fa79a9e908ad6f7b449229153beae68af918"

 #:use-local-checkout #f)

;; It makes no sense to add generation number to the comment. Generation numbers
;; are different on each computer
