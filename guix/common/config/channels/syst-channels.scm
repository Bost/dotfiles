;; `guix pull` places a union of this file with %default-channels to
;; `~/.config/guix/current/manifest`. See
;; guile -c '(use-modules (guix channels)) (format #t "~a\n" %default-channels)'

;;; TODO it seems like the channels are evaluated twice. Once from syst-channels and from home-channels, too.

(define-module (config channels syst-channels))
((@(config channels channel-defs) syst-channels)
 ;; #:guix-commit    "f73e6468c93bf353b1187357fbf10044482dda88"
 ;; #:nonguix-commit "43699ceddb6f5f7aa60a646daa44a75e269a3856"

 ;; 03 sept. 2025 14:06:29
 ;; #:nonguix-commit "477f283914ca771a8622e16b73d845b87c63335d"
 ;; #:guix-commit "b377ec079d9ffe8f0f372c43735ad012ea889b6f"

 ;; 11 sept. 2025 14:29:25
 ;; #:guix-commit "fe2ed12e66097ee2befc55d7ae88e2a7c19f9e72"
 ;; #:nonguix-commit "d096df03564783372b315fe6c179655c2c337d5a"
 )
