;; `guix pull` places a union of this file with %default-channels to
;; `~/.config/guix/current/manifest`. See
;; guile -c '(use-modules (guix channels)) (format #t "~a\n" %default-channels)'

(define-module (config channels home-channels)
  #:use-module (gnu services)           ; simple-service
  #:use-module (gnu home services guix) ; home-channels-service-type

  #:use-module (config channels channel-defs)
  #:use-module (utils)
  #:use-module (memo)
  )

(define-public (home-channels-services)
  (list
   ;; (simple-service 'guixrus-service  home-channels-service-type (list (channel-guixrus)))
   (simple-service 'hask-clj-service home-channels-service-type (list (channel-hask-clj)))
   (simple-service 'bost-service     home-channels-service-type (list (channel-bost))))
  )

(define (home-channels)
  "Channels needed for the Guix-home configuration"
  ((comp
    (lambda (lst)
      (if (or (is-system-edge) (is-system-ecke))
          (append
           (list
            ;; (channel-guix-past) ;; pulled-in via channel-games; not needed directly.
            ;; (channel-guixrus)
            (channel-hask-clj)
            ;; (channel-games)
            ;; (channel-home-service-dwl-guile)
            ;; (channel-flat)
            ;; (channel-rde)
            (channel-bost
             ;; #:commit "6ddbd879a9d6e61cbc58721d00a7b1f380bda57a"

             ;; 04 sept. 2025 15:21:12
             ;; #:commit "5c7b61177d0c71c628687c6e0aa9bf29bbb2f836"
             )
            )
           lst)
          lst)))
   (syst-channels
    ;; Aug 31 2025 14:48:46
    ;; #:nonguix-commit "60ffd0353e70d5e371c4bfe2201c9d08c1c05e18"
    ;; #:guix-commit "d431f4620a4c077383e1168f932e86c99ae33834"

    ;; 03 sept. 2025 14:06:29
    ;; #:nonguix-commit "477f283914ca771a8622e16b73d845b87c63335d"
    ;; #:guix-commit "b377ec079d9ffe8f0f372c43735ad012ea889b6f"

    ;; 05 sept. 2025 15:53:28
    #:nonguix-commit "477f283914ca771a8622e16b73d845b87c63335d"
    #:guix-commit "6bffc03be9dcf4711b5d1e9c95aba340403b35df"
    )))
(testsymb 'home-channels)

(home-channels)
