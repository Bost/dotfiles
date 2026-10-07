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
 ;; 18 septembre 2026 13:03:29
 ;; #:nonguix-commit       "f9171dd0d0a58d63c0811d61e51493a3fa4ae4f3"
 ;; #:bstx-commit          "49335107503e4a0b0755b135cbedbd233f77a16b"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "0daef659a23220fa76a62dcbda7057cb649f415c"

 ;; 2 octobre 2026 17:21:26
 ;; #:nonguix-commit       "c0192e90a52cafb4d33b04734cbe9bbedd703a04"
 ;; #:bstx-commit          "659599cb0a766790adc41f0abfb0773b42b96280"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "8205e4d43b9fd4090643bdebc048699ff60c359d"

 ;; 3 octobre 2026 15:09:21
 ;; #:nonguix-commit       "c0192e90a52cafb4d33b04734cbe9bbedd703a04"
 ;; #:bstx-commit          "df76da5ac5a87c5cddec2f14a0cc533a70fab958"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "8205e4d43b9fd4090643bdebc048699ff60c359d"

 ;; 5 octobre 2026 13:02:49
 ;; #:nonguix-commit       "c0192e90a52cafb4d33b04734cbe9bbedd703a04"
 ;; #:bstx-commit          "2d770a49c8d693b31ccad47c50ce30e4140c0bfd"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "7eccf1b3f2525b909fc9ae5e039c4af7eebe3b05"

 ;; 6 October 2026 12:35:20
 ;; #:nonguix-commit       "c0192e90a52cafb4d33b04734cbe9bbedd703a04"
 ;; #:bstx-commit          "54d3963dec27d1b4b319387a4c5f52b2c12a6675"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "08db59e41162efaaf8991719df155199741eca46"

 ;; 7 October 2026 11:20:35
 ;; #:nonguix-commit       "c0192e90a52cafb4d33b04734cbe9bbedd703a04"
 ;; #:bstx-commit          "daa72b0b09cc336c73ffb4f8e4ee09e02cae9d7c"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "4341c003d7655ac02d72aea58cda706d87d0f965"

 ;; 7 October 2026 17:39:10
 ;; #:nonguix-commit       "c0192e90a52cafb4d33b04734cbe9bbedd703a04"
 ;; #:bstx-commit          "bcf1fd4ab60d47ccfcef16994459a88931f071a3"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "4341c003d7655ac02d72aea58cda706d87d0f965"

 ;; 8 octobre 2026 13:58:57
 #:nonguix-commit       "9b7eb87cb3a8d75de3f1a923f6eb43b2c2c057b0"
 #:bstx-commit          "e0a9c68a9ff7287b139e2aba428ecea0a73212b0"
 #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 #:guix-commit          "a4ddcd15d814d4b7eb147cf7f3ae17cbad122e4e"

 #:use-local-checkout #f)

;; It makes no sense to add generation number to the comment. Generation numbers
;; are different on each computer
