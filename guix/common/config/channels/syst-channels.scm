;; `guix pull` places a union of this file with %default-channels to
;; `~/.config/guix/current/manifest`. See
;; guile -c '(use-modules (guix channels)) (format #t "~a\n" %default-channels)'

(define-module (config channels syst-channels))
((@(config channels channel-defs) syst-channels
  ;; #:guix-commit    "f73e6468c93bf353b1187357fbf10044482dda88"
  ;; #:nonguix-commit "43699ceddb6f5f7aa60a646daa44a75e269a3856"
  ))
