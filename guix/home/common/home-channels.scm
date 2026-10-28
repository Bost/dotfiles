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
 ;; 10 October 2026 19:11
 ;; #:nonguix-commit       "c0e1cd5cd22919ebf2f89fc6578f2169ae9f1323"
 ;; #:bstx-commit          "241ce06bd6fc15d1b55a4113784649a3f8c0f086"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "9a1075521011fe6b6ec40746c9b711a01e002800"

 ;; 14 October 2026 01:08
 ;; #:nonguix-commit       "4b21ba0c885907aff3b1b6c717ac830144d3c575"
 ;; #:bstx-commit          "c672b64e732b0ea16cdd26d6ef86d712b50553df"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "e100c252b4ae4f1d4d93ff1c964b6cd467107bc4"

 ;; 15 October 2026 14:30
 ;; #:nonguix-commit       "4b21ba0c885907aff3b1b6c717ac830144d3c575"
 ;; #:bstx-commit          "99d266e26ef0cdd4ab61f234aba7f917bb35ebc3"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "a6f963401bbe9ced6e1dbea99cb4c0ce50534fce"

 ;; 20 October 2026 18:47
 ;; #:nonguix-commit       "25226c35c150f03d469b0c855d10b2402a094bc1"
 ;; #:bstx-commit          "99d266e26ef0cdd4ab61f234aba7f917bb35ebc3"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "13fd2311536f40d645167215671ed42b211e9c21"

 ;; 24 October 2026 15:27
 ;; #:nonguix-commit       "c241d19ff840dc75556f722b231760d119b6adea"
 ;; #:bstx-commit          "47a468b9a842c5e5ea9f334d7240b317dc22df57"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "973dbc8c00de826f3c38468b09519d30d064eea3"

 ;; 28 October 2026 14:57
 #:nonguix-commit       "c241d19ff840dc75556f722b231760d119b6adea"
 #:bstx-commit          "d18c7b445ac9758da87cec10e32eff1cc8fbac31"
 #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 #:guix-commit          "7c76ac3104534da484372241010dab32a0cf65b2"

 #:use-local-checkout #f)

;; It makes no sense to add generation number to the comment. Generation numbers
;; are different on each computer
