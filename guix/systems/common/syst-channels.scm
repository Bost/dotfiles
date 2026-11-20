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
 ;; 25 October 2026 15:50
 ;; #:bstx-commit    "99d266e26ef0cdd4ab61f234aba7f917bb35ebc3"
 ;; #:nonguix-commit "c241d19ff840dc75556f722b231760d119b6adea"
 ;; #:guix-commit    "973dbc8c00de826f3c38468b09519d30d064eea3"

 ;; 28 October 2026 16:20
 ;; #:nonguix-commit       "c241d19ff840dc75556f722b231760d119b6adea"
 ;; #:bstx-commit          "d18c7b445ac9758da87cec10e32eff1cc8fbac31"
 ;; #:guix-ai-cloud-commit "6e8d113cf0e711dc32481a43f4876a43105c3b17"
 ;; #:guix-commit          "7c76ac3104534da484372241010dab32a0cf65b2"

 ;; 30 October 2026 15:56
 ;; #:bstx-commit    "d18c7b445ac9758da87cec10e32eff1cc8fbac31"
 ;; #:nonguix-commit "c241d19ff840dc75556f722b231760d119b6adea"
 ;; #:guix-commit    "3d3ceb703652b069d5d8d6241f430072e95ddf2a"

 ;; 4 November 2026 02:07
 ;; #:bstx-commit    "d18c7b445ac9758da87cec10e32eff1cc8fbac31"
 ;; #:nonguix-commit "63d83b22615e7a97a4fb0c3d9f6419e9a7fc4fe0"
 ;; #:guix-commit    "b7f6549b5026b5776141482f9e228c49f09b5569"

 ;; 9 November 2026 18:24
 ;; #:bstx-commit    "d18c7b445ac9758da87cec10e32eff1cc8fbac31"
 ;; #:nonguix-commit "775ff3118af3fc7fb77e2a50da970045f0680a0d"
 ;; #:guix-commit    "879a07a3f2c6a2198a98367ec1d1e8bc8d37db48"

 ;; 17 November 2026 16:57
 ;; #:bstx-commit    "d18c7b445ac9758da87cec10e32eff1cc8fbac31"
 ;; #:nonguix-commit "57b156aa003e2b6eacf979cac377e4b585bf3a19"
 ;; #:guix-commit    "634839060dcfdab5819442225a154958b1f3df22"

 ;; 19 November 2026 02:21
 ;; #:bstx-commit    "16a4eb9833d5d21fe4a271f481c7bc1cd74da7a6"
 ;; #:nonguix-commit "57b156aa003e2b6eacf979cac377e4b585bf3a19"
 ;; #:guix-commit    "f012abae216218963a5b1e5275575b4b50847b23"

 ;; 20 November 2026 15:39
 ;; #:bstx-commit    "16a4eb9833d5d21fe4a271f481c7bc1cd74da7a6"
 ;; #:nonguix-commit "57b156aa003e2b6eacf979cac377e4b585bf3a19"
 ;; #:guix-commit    "6ce797d03f395311b559697af381d590942cc52a"

 ;; 20 November 2026 17:45
 #:bstx-commit    "22721bcd98676dd77250c2319f67ca71d7df02ce"
 #:nonguix-commit "57b156aa003e2b6eacf979cac377e4b585bf3a19"
 #:guix-commit    "0a08fa79a9e908ad6f7b449229153beae68af918"

 #:use-local-checkout #f)

;; It makes no sense to add generation number to the comment. Generation numbers
;; are different on each computer
