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
    #:guix-commit    "0b05ca10d77756ae948e79f4a9333b570c6927e5"
    #:nonguix-commit "b5f43404ef9c79220b355e835481749587751512"
    )))
(testsymb 'home-channels)

(home-channels)
