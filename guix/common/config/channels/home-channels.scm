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
            (channel-bost)
            )
           lst)
          lst)))
   (syst-channels
    ;; ;; Generation 1654	Jul 29 2025 12:07:10
    ;; #:guix-commit "806ac0cc045d9b3ea87898bd9343634ee90d2c39"
    ;; #:nonguix-commit "5ed7546593dd205f1dd4473b58afa855c07e033d"
    )))
(testsymb 'home-channels)

(home-channels)
