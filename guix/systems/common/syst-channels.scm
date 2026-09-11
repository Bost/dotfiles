;; `guix pull` places a union of this file with %default-channels to
;; `~/.config/guix/current/manifest`. See
;; guile -c '(use-modules (guix channels)) (format #t "~a\n" %default-channels)'

;; This module is loaded via
;;     --load-path=/home/bost/dev/dotfiles/guix/systems/common
;; so the module-name is not (systems common home-channels)

(define-module (syst-channels)
  #:use-module (dotf config channels channel-defs)
  #:use-module (bost common utils)
  #:use-module (dotf memo)
  )

(define m (module-name-for-logging))
(evaluating-module)

(def* (syst-channels #:key
                     nonguix-commit
                     bstx-commit
                     guix-commit
                     (use-local-checkout #f)
                     #:allow-other-keys
                     )
  ((comp
    (lambda (lst)
      (if (or (host-edge?) (host-ecke?) (host-geek?))
          (append
           (list
            ;; pulls-in (bost common utils)
            (channel-bstx #:commit bstx-commit
                          #:use-local-checkout use-local-checkout)
            (channel-nonguix #:commit nonguix-commit
                             #:use-local-checkout use-local-checkout)
            ) lst)
          lst)))
   (list (channel-guix #:commit guix-commit))))

(module-evaluated)

(syst-channels
 ;; 26 août 2026 12:44:38
 ;; #:nonguix-commit "c15e19cdbdfdfddacdae865741809af4fa86a665"
 ;; #:bstx-commit    "e5194b1fc1a9e417bfabfd65f692294fdef2d9be"
 ;; #:guix-commit    "272d526d58e6eeb215dd0232e006846de78ed6e1"

 ;; 02 sept. 2026 15:20:32
 ;; #:bstx-commit    "c43512e5959601efb7cc2584dfae852028653f5b"
 ;; #:nonguix-commit "accdba77be815bbe00868145dd66d68056eecf7a"
 ;; #:guix-commit    "b8422bc92c56c5c7f22a9378b5ac7695bc487fb4"

 ;; 05 sept. 2026 15:05:20
 ;; #:bstx-commit    "388a77283a2b74cf7e1b4826be60473b40ebc114"
 ;; #:nonguix-commit "accdba77be815bbe00868145dd66d68056eecf7a"
 ;; #:guix-commit    "99cd3f04169d0eef03b5fc7e3a9e2fec6542c3aa"

 ;; 07 sept. 2026 12:00:15
 ;; #:bstx-commit    "49335107503e4a0b0755b135cbedbd233f77a16b"
 ;; #:nonguix-commit "accdba77be815bbe00868145dd66d68056eecf7a"
 ;; #:guix-commit    "d759a1922126909b6097e245631669cf0b368b57"

 ;; 08 sept. 2026 21:25:45
 #:bstx-commit    "49335107503e4a0b0755b135cbedbd233f77a16b"
 #:nonguix-commit "accdba77be815bbe00868145dd66d68056eecf7a"
 #:guix-commit    "ab3cc11100a7eeff65b9a36d849d0fb96dda4eb9"


 #:use-local-checkout #f)

;; It makes no sense to add generation number to the comment. Generation numbers
;; are different on each computer
