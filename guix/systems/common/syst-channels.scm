;; `guix pull` places a union of this file with %default-channels to
;; `~/.config/guix/current/manifest`. See
;; guile -c '(use-modules (guix channels)) (format #t "~a\n" %default-channels)'

;; This module is loaded via
;;     --load-path=/home/bost/dev/dotfiles/guix/systems/common
;; so the module-name is not (systems common home-channels)

(define-module (syst-channels)
  #:use-module (dotf config channels channel-defs)
  )

(define (syst-channels)
  (common-channels
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
   #:guix-commit "d88ecb34b4ec6ce43e34def8e54b2f2522706721"
   #:nonguix-commit "ba91bc437dba367f98608658bb4a19fb0880ad24"
   ))

(syst-channels)

;; TODO fix deprecations
;; 'keybinder-3.0' is deprecated, use 'keybinder' instead
;; 'rofi-wayland' is deprecated, use 'rofi' instead
;; 'ruby-concurrent' is deprecated, use 'ruby-concurrent-ruby' instead
