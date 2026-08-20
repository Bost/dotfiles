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
 ;; 04 août 2026 18:44:40
 ;; #:bstx-commit    "82b0707493422da2c9eab085e2df6bc1ce5baee9"
 ;; #:nonguix-commit "7b7b2c47f9c205ad89ddf54293e7756e797f8980"
 ;; #:guix-commit    "d3acc7d021c51e292fe7572d6eef3d2d8d86b1c2"

 ;; 06 août 2026 11:07:24
 ;; #:bstx-commit    "7131259e2c05a04d9e95f1ecbc61d1dcc68677bc"
 ;; #:nonguix-commit "7b7b2c47f9c205ad89ddf54293e7756e797f8980"
 ;; #:guix-commit    "e52280b9b64fdf02a4a6cd738896f3a10fc51470"

 ;; 07 août 2026 23:14:28
 ;; #:bstx-commit    "163e1d1c9602b602e0ff676f8ea567d2b6a98ba0"
 ;; #:nonguix-commit "653504e6551198c9b2b998c143d7cf2675b22547"
 ;; #:guix-commit    "fea672ffdae11b050fc5cb986ff2ad86f54ed1d1"

 ;; Aug 09 2026 22:50:19
 ;; #:nonguix-commit "653504e6551198c9b2b998c143d7cf2675b22547"
 ;; #:bstx-commit    "c5c4c82e1ad98d98c46720d19dbf4216dda759e5"
 ;; #:guix-commit    "b9a35ee6b53438d53d66cbe3e82f26bb22a44b5b"

 ;; 10 août 2026 14:36:35
 ;; #:nonguix-commit "653504e6551198c9b2b998c143d7cf2675b22547"
 ;; #:bstx-commit    "ceb0df364a22f0d2e249316a5736771b048ca91b"
 ;; #:guix-commit    "714b87a5955f590b79c4f54c071f3f54b66c3c50"

 ;; 11 août 2026 14:08:13
 ;; #:nonguix-commit "653504e6551198c9b2b998c143d7cf2675b22547"
 ;; #:bost-commit    "b2c3b690122e0afec40ade444611df2f515758ca"
 ;; #:guix-commit    "f0332077b948a72f79c16b0838c3c7cdddaed631"

 ;; 20 août 2026 22:51:22
 #:nonguix-commit "653504e6551198c9b2b998c143d7cf2675b22547"
 #:bstx-commit    "cca04f96125424bed31a59d3e9461da8518c7ba5"
 #:guix-commit    "c98ec501cce5c4776602ae7cb90b0ba5962ee895"

 #:use-local-checkout #f)

;; It makes no sense to add generation number to the comment. Generation numbers
;; are different on each computer
