;;; Shared package-lookup helpers usable from both `guix home reconfigure' and
;;; `guix system reconfigure'. Loaded via
;;;     --load-path=/home/bost/dev/dotfiles/guix/common
;;; which is on the load path of both:
;;;     guix/home/*.scm
;;;     guix/systems/*.scm and guix/systems/common/syst-base.scm

(define-module (dotf config packages package-defs)
  #:use-module (bost common utils)

  ;; for pkg-or-inferior : beg
  #:use-module (guix packages)
  #:use-module (guix inferior)
  #:use-module (guix channels)
  ;; #:use-module (guix profiles) ;; probably not needed
  ;; for pkg-or-inferior : end

  #:use-module (srfi srfi-1)  ; first
  )

(define m (module-name-for-logging))
(evaluating-module)

;; cat /var/guix/profiles/per-user/$USER/guix-profile-<profile-number>-link/manifest
(def*-public (pkg-or-inferior package #:key (channels '()))
  "Return PACKAGE as-is, unless CHANNELS is non-empty, in which case look up
and return the same-named package from the inferior built from CHANNELS - a
list of channel objects, e.g. (list (channel-guix #:commit \"...\")).

To switch between the ordinary and the inferior package, comment/uncomment
the channel entries inside #:channels - no separate flag needed."
  (if (empty? channels)
      package
      (first (lookup-inferior-packages
              (inferior-for-channels channels)
              (package-name package)))))
(testsymb 'pkg-or-inferior)

(module-evaluated)
