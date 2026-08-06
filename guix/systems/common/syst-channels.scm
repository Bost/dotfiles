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
                     bost-commit
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
            (channel-bost #:commit bost-commit
                          #:use-local-checkout use-local-checkout)
            (channel-nonguix #:commit nonguix-commit
                             #:use-local-checkout use-local-checkout)
            ) lst)
          lst)))
   (list (channel-guix #:commit guix-commit))))

(module-evaluated)

(syst-channels
 ;; 11 juin 2026 18:44:42
 ;; #:nonguix-commit    "4ae06fb5cb75f2ca6b0f2f384f41677ae28c069a"
 ;; #:guix-commit       "e494c2bd3de8087ac19c1fce9effb3128b35091e"

 ;; 13 juin 2026 13:20:25
 ;; #:nonguix-commit    "4ae06fb5cb75f2ca6b0f2f384f41677ae28c069a"
 ;; #:guix-commit       "4a50b4bc65758f2917774f335de4c4586baa72fc"

 ;; 18 juin 2026 14:51:15
 ;; #:nonguix-commit    "bf39542ca537fde8839b209ac21d6f3254469b15"
 ;; #:guix-commit       "d0580550cd4a3a34aab31d631751ed5847e42976"

 ;; Jun 22 2026 16:50:09
 ;; #:nonguix-commit "bf39542ca537fde8839b209ac21d6f3254469b15"
 ;; #:guix-commit    "acd7100df96a4de2e702072c24b0445438c0c9a7"

 ;; 24 juin 2026 13:57:36
 ;; #:nonguix-commit "bf39542ca537fde8839b209ac21d6f3254469b15"
 ;; #:guix-commit    "6d7a9269eac93919f050ff0146e261e1b27d1cbe"

 ;; Jul 01 2026 15:03:49
 ;; #:nonguix-commit "d35a2f8f22023426ccf3598fa7079b09bb821e3e"
 ;; #:guix-commit    "38031dd2c8b08bb21cc429f981a2ca843c205bd5"

 ;; Jul 04 2026 14:29:59
 ;; #:nonguix-commit "4bc86c61d5ab661614b099bfe524f7f5798988b3"
 ;; #:guix-commit    "a118e78776390b1e56928927d5056cd9426d786e"

 ;; Jul 09 2026 12:23:08
 ;; #:nonguix-commit "fe63493aba7ad6107cb938fcd307c400b53a32b1"
 ;; #:guix-commit    "09a208a590dfc0fc49cd7f325cd2a0fd6c9d5c12"

 ;; 15 July 2026 17:16:06
 ;; #:nonguix-commit "3b66965566fe8c96edb5a41fd39a9e5a90ad9b61"
 ;; #:guix-commit    "b1c803d57f4acd95bd802dd6d5e65db9bdc5b0c0"

 ;; 24 juil. 2026 12:32:36
 ;; #:nonguix-commit "3b66965566fe8c96edb5a41fd39a9e5a90ad9b61"
 ;; #:guix-commit    "65272f778a1aa54f3836c8640aded44267af3241"

 ;; 29 juil. 2026 17:24:15
 ;; #:nonguix-commit "73baab37361b3a81f326aa3fdec78840f5acc577"
 ;; #:guix-commit    "dcddca760b8252474498e549b6e6ef34fc393f21"

 ;; 04 août 2026 18:44:40
 ;; #:bost-commit    "82b0707493422da2c9eab085e2df6bc1ce5baee9"
 ;; #:nonguix-commit "7b7b2c47f9c205ad89ddf54293e7756e797f8980"
 ;; #:guix-commit    "d3acc7d021c51e292fe7572d6eef3d2d8d86b1c2"

 ;; 06 août 2026 11:07:24
 #:bost-commit    "7131259e2c05a04d9e95f1ecbc61d1dcc68677bc"
 #:nonguix-commit "7b7b2c47f9c205ad89ddf54293e7756e797f8980"
 #:guix-commit    "e52280b9b64fdf02a4a6cd738896f3a10fc51470"

 #:use-local-checkout #f)

;; It makes no sense to add generation number to the comment. Generation numbers
;; are different on each computer
