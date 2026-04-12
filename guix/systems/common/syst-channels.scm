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
                     #:allow-other-keys
                     )
  ((comp
    (lambda (lst)
      (if (or (host-edge?) (host-ecke?) (host-geek?))
          (append
           (list
            (channel-guix-science #:commit guix-science-commit)
            (channel-nonguix #:commit nonguix-commit)
            ) lst)
          lst)))
   (list (channel-guix #:commit guix-commit))))

(module-evaluated)

(syst-channels
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
 ;; #:nonguix-commit "1980960f932063f42f97ad3be4b020f68d24e62b"
 ;; #:guix-commit "0b6b8a1e88ada49cb3303be8d4eefe95dca704dd"

 ;; 26 feb. 2026 14:22:42
 ;; #:nonguix-commit "da4e72efef62d48dbc2eb089c36972ff55fe6acd"
 ;; #:guix-commit "c07690f0d673d8415066160617455313afa1d544"

 ;; 04 mar. 2026 20:53:35
 ;; #:nonguix-commit "816b5c6f45ed56cd81e2a6338ed5b710e4e66e98"
 ;; #:guix-commit "a375202ad3443e4c3fb492160e32aa6abcc965bd"

 ;; 15 mar. 2026 12:05:35
 #:nonguix-commit "7d14e819fc7b2681240023b9e4c001fc3f5cba04"
 #:guix-commit "a375202ad3443e4c3fb492160e32aa6abcc965bd"
 )
