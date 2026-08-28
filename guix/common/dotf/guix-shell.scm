;;; Home-relative helpers for `guix shell --container', built on top of
;;; (bost common guix-shell)'s dependency-light argv-flag builders, which
;;; this module re-exports so callers only need to import this one module
;;; for everything guix-shell-related.
;;;
;;; These two helpers `guix-home-mapping'/ `guix-share-home-path' remain here
;;; rather than in the bstx channel because they depend on `user-home', which
;;; lives in this same (dotf fs-utils).

(define-module (dotf guix-shell)
  #:use-module (bost common guix-shell)
  #:use-module (dotf fs-utils) ; user-home
  #:re-export
  (guix-preserve-exact
   guix-share
   guix-share-as
   guix-expose
   guix-expose-as
   guix-expose-if-exists))

(define-public (guix-home-mapping host-path container-relative-path)
  "Pair HOST-PATH with the user's home-relative CONTAINER-RELATIVE-PATH, e.g.
for use with `guix-share-as'/`guix-expose-as':

(guix-home-mapping \"/foo\" \".config/bar\")
  ;=> (\"/foo\" \"/home/bost/.config/bar\")"
  (list host-path (user-home "/" container-relative-path)))

(define-public (guix-share-home-path path)
  "Like `guix-home-mapping', but maps PATH on the host to the identically named
path under the user's home inside the container, returning a `guix-share-as'
flag string."
  (apply guix-share-as (guix-home-mapping path path)))
