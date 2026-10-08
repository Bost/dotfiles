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
 ;; 18 sept. 2026 13:31:15
 ;; #:bstx-commit    "49335107503e4a0b0755b135cbedbd233f77a16b"
 ;; #:nonguix-commit "f9171dd0d0a58d63c0811d61e51493a3fa4ae4f3"
 ;; #:guix-commit    "0daef659a23220fa76a62dcbda7057cb649f415c"

 ;; 02 oct. 2026 17:48:49
 ;; #:nonguix-commit "c0192e90a52cafb4d33b04734cbe9bbedd703a04"
 ;; #:bstx-commit    "659599cb0a766790adc41f0abfb0773b42b96280"
 ;; #:guix-commit    "8205e4d43b9fd4090643bdebc048699ff60c359d"

 ;; 04 oct. 2026 01:19:31
 ;; #:bstx-commit    "df76da5ac5a87c5cddec2f14a0cc533a70fab958"
 ;; #:nonguix-commit "c0192e90a52cafb4d33b04734cbe9bbedd703a04"
 ;; #:guix-commit    "8205e4d43b9fd4090643bdebc048699ff60c359d"

 ;; 05 oct. 2026 13:16:23
 ;; #:bstx-commit    "2d770a49c8d693b31ccad47c50ce30e4140c0bfd"
 ;; #:nonguix-commit "c0192e90a52cafb4d33b04734cbe9bbedd703a04"
 ;; #:guix-commit    "7eccf1b3f2525b909fc9ae5e039c4af7eebe3b05"

 ;; Oct 06 2026 15:38:23
 ;; #:bstx-commit    "54d3963dec27d1b4b319387a4c5f52b2c12a6675"
 ;; #:nonguix-commit "c0192e90a52cafb4d33b04734cbe9bbedd703a04"
 ;; #:guix-commit    "08db59e41162efaaf8991719df155199741eca46"

 ;; Oct 07 2026 11:32:56
 ;; #:bstx-commit    "daa72b0b09cc336c73ffb4f8e4ee09e02cae9d7c"
 ;; #:nonguix-commit "c0192e90a52cafb4d33b04734cbe9bbedd703a04"
 ;; #:guix-commit    "4341c003d7655ac02d72aea58cda706d87d0f965"

 ;; Oct 07 2026 17:50:06
 ;; #:bstx-commit    "bcf1fd4ab60d47ccfcef16994459a88931f071a3"
 ;; #:nonguix-commit "c0192e90a52cafb4d33b04734cbe9bbedd703a04"
 ;; #:guix-commit    "4341c003d7655ac02d72aea58cda706d87d0f965"

 ;; 9 October 2026 00:15
 #:bstx-commit    "3436ce2229b9c7cce0db99a2a7b0ab7ffe8164b5"
 #:nonguix-commit "9b7eb87cb3a8d75de3f1a923f6eb43b2c2c057b0"
 #:guix-commit    "a4ddcd15d814d4b7eb147cf7f3ae17cbad122e4e"

 #:use-local-checkout #f)

;; It makes no sense to add generation number to the comment. Generation numbers
;; are different on each computer
