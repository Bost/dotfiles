;; `guix pull` places a union of this file with %default-channels to
;; `~/.config/guix/current/manifest`. See
;; guile -c '(use-modules (guix channels)) (format #t "~a\n" %default-channels)'

;; This module is loaded via
;;     --load-path=/home/bost/dev/dotfiles/guix/systems/common
;; so the module-name is not (systems common home-channels)

(define-module (syst-channels)
  #:use-module (dotf config channels channel-defs)
  #:use-module (dotf utils)
  #:use-module (dotf memo)
  )

(define m (module-name-for-logging))
(evaluating-module)

(def* (syst-channels #:key
                     guix-science-commit
                     guix-commit
                     nonguix-commit
                     (use-local-checkout #f)
                     #:allow-other-keys
                     )
  ((comp
    (lambda (lst)
      (if (or (host-edge?) (host-ecke?) (host-geek?))
          (append
           (list
            ;; (channel-guix-science #:commit guix-science-commit)
            (channel-nonguix #:commit nonguix-commit
                             #:use-local-checkout use-local-checkout)
            ) lst)
          lst)))
   (list (channel-guix #:commit guix-commit))))

(module-evaluated)

(syst-channels
 ;; 06 mai 2026 12:02:11
 ;; #:guix-science-commit "4f29b99ce090ed2992d5bc6f3af42180201f5058"
 ;; #:nonguix-commit      "5f2630e69fbbe9e79c350a67545f0fef7e93e223"
 ;; #:guix-commit         "81934cf7e9263e47f9fd8defa717365592a3473d"

 ;; 10 mai 2026 14:05:19
 ;; #:guix-science-commit "0c0df2b83829ba5cd1fa5ca58c861062a7d20c02"
 ;; #:nonguix-commit      "5f2630e69fbbe9e79c350a67545f0fef7e93e223"
 ;; #:guix-commit         "9f8d0595a2c044a9bc1c97893b338c485ee83c18"

 ;; 18 mai 2026 17:43:45
 ;; #:guix-science-commit "24317029245eddca35f64b81280a831a18bc680c"
 ;; #:nonguix-commit      "5f2630e69fbbe9e79c350a67545f0fef7e93e223"
 ;; #:guix-commit         "928f7ff31a61d6e3f764d6ce32bfe3bb4c9db9e8"

 ;; 21 mai 2026 12:48:04
 ;; #:nonguix-commit      "5f2630e69fbbe9e79c350a67545f0fef7e93e223"
 ;; #:guix-science-commit "76200c5a049c59eb9f7622cbafc20bafddf3750a"
 ;; #:guix-commit         "1672274e6f2bb20fb72bc8150ce4d0225281ac6b"

 ;; 22 mai 2026 14:25:04
 ;; #:nonguix-commit      "0107c1c4849460b567424ba2cdc8942294c7a611"
 ;; #:guix-science-commit "76200c5a049c59eb9f7622cbafc20bafddf3750a"
 ;; #:guix-commit         "e6b49f7d47cb1c38bfeba661a5e5cc6d1d17c4f4"

 ;; May 23 2026 20:08:14
 ;; #:nonguix-commit      "0107c1c4849460b567424ba2cdc8942294c7a611"
 ;; #:guix-commit         "0b597e551715ca6cbdbb2ffeacd530624a01d200"

 ;; 25 mai 2026 22:47:07
 ;; #:nonguix-commit      "ffa33d200e1c930e79d00021f03b2f8c31d00b61"
 ;; #:guix-commit         "8c95d6364a34cfbd60efbd15d85ce8b02ff6a2cd"

 ;; 27 mai 2026 17:21:12
 ;; #:nonguix-commit      "e1273e751bb4a65cd8f817b871bfde740373d917"
 ;; #:guix-commit         "19da15f8713af74f1f7fec0bc3864efa92f43129"

 ;; 28 mai 2026 15:32:02
 ;; #:nonguix-commit      "e1273e751bb4a65cd8f817b871bfde740373d917"
 ;; #:guix-commit         "48dda47112f2e427ca17c9ccfa6ce83c026e228d"

 ;; 29 mai 2026 13:39:37
 ;; #:nonguix-commit      "615f33a7ae0edd58ebeb412da15c56c4757e5547"
 ;; #:guix-commit         "33acdbdfb19aff97150c7922ed97e250825f566b"

 ;; 30 mai 2026 13:45:26
 ;; #:nonguix-commit      "01ccd92a73434029119c0ef1bd8e0f1a7bb71a8e"
 ;; #:guix-commit         "33acdbdfb19aff97150c7922ed97e250825f566b"

 ;; 30 mai 2026 15:04:49
 ;; #:nonguix-commit      "3ed7c207c59dde11a97db483cad4c96eae1a10c4"
 ;; #:guix-commit         "c905bdf006e13ac1bf8f2ff375674d7cfa484bac"

 ;; 11 juin 2026 18:44:42
 ;; #:nonguix-commit    "4ae06fb5cb75f2ca6b0f2f384f41677ae28c069a"
 ;; #:guix-commit       "e494c2bd3de8087ac19c1fce9effb3128b35091e"

 ;; 13 juin 2026 13:20:25
 ;; #:nonguix-commit    "4ae06fb5cb75f2ca6b0f2f384f41677ae28c069a"
 ;; #:guix-commit       "4a50b4bc65758f2917774f335de4c4586baa72fc"

 ;; 18 juin 2026 14:51:15
 #:nonguix-commit    "bf39542ca537fde8839b209ac21d6f3254469b15"
 #:guix-commit       "d0580550cd4a3a34aab31d631751ed5847e42976"

 #:use-local-checkout #f)

;; It makes no sense to add generation number to the comment. Generation numbers
;; are different on each computer
