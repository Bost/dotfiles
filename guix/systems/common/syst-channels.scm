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
                     guix-commit
                     nonguix-commit
                     #:allow-other-keys
                     )
  ((comp
    (lambda (lst)
      (if (or (host-edge?) (host-ecke?) (host-geek?))
          (append
           (list (channel-nonguix #:commit nonguix-commit)) lst)
          lst)))
   (list (channel-guix #:commit guix-commit))))

(module-evaluated)

(syst-channels
 ;; 25 sept. 2025 19:54:02
 ;; #:guix-commit "ace78713a9ee7abfdd19e2f009c3fb606b797160"
 ;; #:nonguix-commit "25d7a8091c2c9678a8694f073d846a7001165169"

 ;; 26 sept. 2025 13:15:27
 ;; #:guix-commit "3c96d9fb1e1ee8fe4efd44ba6e8f3886b43668be"
 ;; #:nonguix-commit "25d7a8091c2c9678a8694f073d846a7001165169"

 ;; 07 oct. 2025 16:38:18
 ;; #:guix-commit "2793cb63025047fe2a9c7b3f05007475d4a1c6ab"
 ;; #:nonguix-commit "a5d216cd7c2d67eb95e58871bb805f22c160c57b"

 ;; 13 oct. 2025 16:29:31
 ;; #:guix-commit "c17d65c728c571668b84f82ad7cb8649d34019b5"
 ;; #:nonguix-commit "3f4a1907cae9b0def09d1549c491bb4e307b9097"

 ;; 22 oct. 2025 16:43:15
 ;; #:guix-commit "f01920e7f4962896452126928ea45a3b9e31832b"
 ;; #:nonguix-commit "a345ef84fbdf3b2491acb2c2b6665a4eb97bd4aa"

 ;; 03 nov. 2025 00:09:24
 ;; #:guix-commit "4fd32c00abb59b0f275a93c3fef35d5193adbe7a"
 ;; #:nonguix-commit "971bc275cf8a6554003ca924c65142ab7d430cad"

 ;; 14 nov. 2025 14:24:12
 ;; #:guix-commit "7e286874b81dc1d4f60153ae80348e493eda35c1"
 ;; #:nonguix-commit "0f68c1684169cbef8824fb246dfefa3e6832225b"

 ;; 19 nov. 2025 14:37:33
 ;; #:guix-commit "7e286874b81dc1d4f60153ae80348e493eda35c1"
 ;; #:nonguix-commit "0f68c1684169cbef8824fb246dfefa3e6832225b"

 ;; 02 déc. 2025 19:01:47
 ;; #:guix-commit "5f7cc5c2c6497ebaf7039cdb30ceba025602d698"
 ;; #:nonguix-commit "82be0b7adaaaa7a98d47382d7f72dd2e31d8e6d8"

 ;; 10 déc. 2025 22:25:49
 ;; #:guix-commit "ec959892550ecbfcd4be47e8464df953af6529b3"
 ;; #:nonguix-commit "cb35c71a028eb4a7c950c6a9637c0efad413ef35"

 ;; 29 déc. 2025 17:05:40
 ;; xfce is broken
 ;; #:guix-commit "d88ecb34b4ec6ce43e34def8e54b2f2522706721"
 ;; #:nonguix-commit "ba91bc437dba367f98608658bb4a19fb0880ad24"

 ;; 05 jan. 2026 12:29:08
 ;; #:guix-commit "b670a1ed28d8e9e31a8f698066eda39200ad5f5d"
 ;; #:nonguix-commit "cb2d0277635eaf4c573aeb9ca7563605fac7601c"

 ;; 12 jav. 2026 12:04:45
 ;; #:guix-commit "62c28bc6d8c54800b16341d1c93a1d7833fc3328"
 ;; #:nonguix-commit "3ceef556e1ce8e52180b959a942b16000ed4e61c"

 ;; 30 jan. 2026 02:11:55
 ;; #:guix-commit "930ec113149f343c160349eb78044dab57d6fea5"
 ;; #:nonguix-commit "216cf961b56a8b2679239ba102089ab94c259e89"

 ;; 30 jav. 2026 19:40:42
 ;; #:guix-commit "a6dddbb062ebc9ed20a51dbec0f1f2e9b6dba77c"
 ;; #:nonguix-commit "216cf961b56a8b2679239ba102089ab94c259e89"

 ;; 05 feb. 2026 14:35:03
 ;; #:nonguix-commit "6c0ea215e0bd089bf3b2097e5c59dd726fbbe304"
 ;; #:guix-commit "2c710762f5c80b1a151b57c6f369dfa1812d1f97"

 ;; 07 feb. 2026 12:08:23
 ;; #:nonguix-commit "6c0ea215e0bd089bf3b2097e5c59dd726fbbe304"
 ;; #:guix-commit "ec5fb6678f8268437b1940f7ed2f2b72d62ab4e0"

 ;; 15 feb. 2026 13:47:37
 ;; #:nonguix-commit "bbbc850e8a94833c9958625eb4c78a206fbacc1d"
 ;; #:guix-commit "7113a63ff4bdcf24fbc4bb92367ea6354dd33290"

 ;; 18 feb. 2026 18:07:07
 #:nonguix-commit "1980960f932063f42f97ad3be4b020f68d24e62b"
 #:guix-commit "0b6b8a1e88ada49cb3303be8d4eefe95dca704dd"
 )
