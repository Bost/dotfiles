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
 ;; Oct 07 2026 11:32:56
 ;; #:bstx-commit    "daa72b0b09cc336c73ffb4f8e4ee09e02cae9d7c"
 ;; #:nonguix-commit "c0192e90a52cafb4d33b04734cbe9bbedd703a04"
 ;; #:guix-commit    "4341c003d7655ac02d72aea58cda706d87d0f965"

 ;; Oct 07 2026 17:50:06
 ;; #:bstx-commit    "bcf1fd4ab60d47ccfcef16994459a88931f071a3"
 ;; #:nonguix-commit "c0192e90a52cafb4d33b04734cbe9bbedd703a04"
 ;; #:guix-commit    "4341c003d7655ac02d72aea58cda706d87d0f965"

 ;; 9 October 2026 00:15
 ;; #:bstx-commit    "3436ce2229b9c7cce0db99a2a7b0ab7ffe8164b5"
 ;; #:nonguix-commit "9b7eb87cb3a8d75de3f1a923f6eb43b2c2c057b0"
 ;; #:guix-commit    "a4ddcd15d814d4b7eb147cf7f3ae17cbad122e4e"

 ;; 10 October 2026 19:37
 ;; #:bstx-commit    "241ce06bd6fc15d1b55a4113784649a3f8c0f086"
 ;; #:nonguix-commit "c0e1cd5cd22919ebf2f89fc6578f2169ae9f1323"
 ;; #:guix-commit    "9a1075521011fe6b6ec40746c9b711a01e002800"

 ;; 14 October 2026 01:27
 ;; #:bstx-commit    "c672b64e732b0ea16cdd26d6ef86d712b50553df"
 ;; #:nonguix-commit "4b21ba0c885907aff3b1b6c717ac830144d3c575"
 ;; #:guix-commit    "e100c252b4ae4f1d4d93ff1c964b6cd467107bc4"

 ;; 15 October 2026 15:16
 ;; #:bstx-commit    "99d266e26ef0cdd4ab61f234aba7f917bb35ebc3"
 ;; #:nonguix-commit "4b21ba0c885907aff3b1b6c717ac830144d3c575"
 ;; #:guix-commit    "a6f963401bbe9ced6e1dbea99cb4c0ce50534fce"

 ;; 20 October 2026 20:48
 ;; #:bstx-commit    "99d266e26ef0cdd4ab61f234aba7f917bb35ebc3"
 ;; #:nonguix-commit "25226c35c150f03d469b0c855d10b2402a094bc1"
 ;; #:guix-commit    "13fd2311536f40d645167215671ed42b211e9c21"

 ;; 25 October 2026 15:50
 #:bstx-commit    "99d266e26ef0cdd4ab61f234aba7f917bb35ebc3"
 #:nonguix-commit "c241d19ff840dc75556f722b231760d119b6adea"
 #:guix-commit    "973dbc8c00de826f3c38468b09519d30d064eea3"

 #:use-local-checkout #f)

;; It makes no sense to add generation number to the comment. Generation numbers
;; are different on each computer
