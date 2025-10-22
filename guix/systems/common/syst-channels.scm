;; `guix pull` places a union of this file with %default-channels to
;; `~/.config/guix/current/manifest`. See
;; guile -c '(use-modules (guix channels)) (format #t "~a\n" %default-channels)'

;; This module is loaded via
;;     --load-path=/home/bost/dev/dotfiles/guix/systems/common
;; so the module-name is not (systems common home-channels)

(define-module (syst-channels)
  #:use-module (config channels channel-defs)
  )

(define (syst-channels)
  (common-channels
   ;; #:guix-commit    "f73e6468c93bf353b1187357fbf10044482dda88"
   ;; #:nonguix-commit "43699ceddb6f5f7aa60a646daa44a75e269a3856"

   ;; 03 sept. 2025 14:06:29
   ;; #:nonguix-commit "477f283914ca771a8622e16b73d845b87c63335d"
   ;; #:guix-commit "b377ec079d9ffe8f0f372c43735ad012ea889b6f"

   ;; 11 sept. 2025 14:29:25
   ;; #:guix-commit "fe2ed12e66097ee2befc55d7ae88e2a7c19f9e72"
   ;; #:nonguix-commit "d096df03564783372b315fe6c179655c2c337d5a"

   ;; 22 sept. 2025 17:56:58
   ;; #:guix-commit "f662d534068736d0770f9988a29e268db2db2a51"
   ;; #:nonguix-commit "bc2a73d7a8252b14a2a3705f8df791ab314f5a5c"

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
   #:guix-commit "f01920e7f4962896452126928ea45a3b9e31832b"
   #:nonguix-commit "a345ef84fbdf3b2491acb2c2b6665a4eb97bd4aa"
   ))

(syst-channels)
