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
             )
            )
           lst)
          lst)))
   (syst-channels
    ;; Jul 29 2025 12:07:10
    ;; #:guix-commit "806ac0cc045d9b3ea87898bd9343634ee90d2c39"
    ;; #:nonguix-commit "5ed7546593dd205f1dd4473b58afa855c07e033d"

    ;; Aug 23 2025 23:24:30 - doesn't work at the moment
    ;; #:nonguix-commit "9bf3d1394f595413c51d621e61f231789c74891e"
    ;; #:guix-commit "64ef6db09b1e14a12fc0d97a5a0615394d6d6c7a"

    ;; Aug 31 2025 14:48:46
    ;; #:nonguix-commit "60ffd0353e70d5e371c4bfe2201c9d08c1c05e18"
    ;; #:guix-commit "d431f4620a4c077383e1168f932e86c99ae33834"
    )))
(testsymb 'home-channels)

(home-channels)
