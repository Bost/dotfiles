#|
# Create profile: run in /bin/sh
mkdir $GUIX_EXTRA_PROFILES
source /etc/profile
guix package --cores=24 \
    -L /home/bost/dev/dotfiles/guix/common \
    -L /home/bost/dev/dotfiles/guix/home/common \
    --profile=$GUIX_EXTRA_PROFILES/emacs-29.1 \
    --manifest=$dotf/guix/manifest-emacs-29.1.scm

# Activate the profile:
GUIX_PROFILE="/home/bost/.guix-extra-profiles/emacs-29.1"
. "$GUIX_PROFILE/etc/profile"
|#
(define-module (manifest-emacs-29.1)
  #:use-module (srfi srfi-1) ;; remove
  #:use-module (guix packages)
  #:use-module (utils) ;; partial
  #:use-module (guix profiles)
  #:use-module (cfg packages all) ;; packages-to-install

  ;; for inferior-package-in-guix-channel : beg
  #:use-module (guix packages)
  #:use-module (guix inferior)
  #:use-module (guix channels)
  ;; #:use-module (guix profiles) ;; probably not needed
  ;; for inferior-package-in-guix-channel : end
  )

(define m (module-name-for-logging))
(evaluating-module)

(define my-package? (@(guix packages) package?))
(define my-package-name (@(guix packages) package-name))
(define my-package->manifest-entry (@(guix profiles) package->manifest-entry))

(define my-inferior-package? (@(guix inferior) inferior-package?))
(define my-inferior-package-name (@(guix inferior) inferior-package-name))
(define my-inferior-package->manifest-entry (@(guix inferior) inferior-package->manifest-entry))

(define* (dbg-packages-to-install #:key search-space pkgs)
  ((comp
    (lambda (p)
      (unless (unspecified-or-empty-or-false? p)
        (format #t "D ~a Packages in the search-space: ~a :\n~a\n"
                m search-space p))
      p)
    (partial
     filter
     ;; see also `string=?', `eq?', `equal?', etc.
     ;; member uses `equal?'
     (lambda (p)
       (cond
        [(my-package? p) (member (my-package-name p) search-space)]
        [(list? p)       (member (my-package-name (car p)) search-space)]
        [(string? p)     (member (my-package-name p) search-space)]
        [(record? p)     (member (my-inferior-package-name p) search-space)]
        [else            (member (my-package-name p) search-space)])))
    (partial
     map
     (lambda (p)
       (cond ;; we have a colorful mix here
        [(list? p)
         (when (member (my-package-name (car p)) search-space)
           (format #t "D Object is a list: ~a\n" p))]
        [(string? p)
         (when (member (my-package-name p) search-space)
           (format #t "D Object is a string: ~a\n" p))]
        [(my-package? p)
         (when (member (my-package-name p) search-space)
           (format #t "D Object is a package: ~a\n" p))]
        [(record? p)
         (when (member (my-inferior-package-name p) search-space)
           (format #t "D Object is a record: ~a\n" p))]
        [else
         (when (member (my-package-name p) search-space)
           (format #t "D Object is something else: ~a\n" p))])
       p))
    identity)
   pkgs)
  (format #t "I ~a Packages to install: ~a\n" m (length pkgs))
  pkgs)

(define-public (to-manifest-entry package)
  (cond
   [(list? package)
    (apply my-package->manifest-entry #;to-manifest-entry package)]
   [(my-inferior-package? package)
    (my-inferior-package->manifest-entry package)]
   #;
   [(my-package? package)
    (my-package->manifest-entry package)]
   [else
    (my-package->manifest-entry package)]))

(define-public (inferior-package-in-guix-channel package commit)
  "Returns an inferior representing the `commit' (predecessor-sha1) revision.
Can't be in the guix/common/utils.scm. Therefore duplicated.
See guix/manifest-emacs-29.1.scm, guix/home/common/cfg/packages/all.scm"
  (first
   (lookup-inferior-packages
    (inferior-for-channels
     (list (channel
            (name 'guix)
            (url "https://git.savannah.gnu.org/git/guix.git")
            (commit commit))))
    package)))
(testsymb 'inferior-package-in-guix-channel)

(define (inferior-pkgs pkgs)
  "The original, i.e. non-inferior packages must not be present in the PKGS."
  ((comp
    (partial append pkgs)
    ;; (lambda (pkgs) (format #t "~a\ninferior-pkgs: ~a\n" m pkgs) pkgs)
    (partial map (partial apply inferior-package-in-guix-channel)))
   (list
    ;; emacs 29.1
    (list "emacs" "ff1ec930e5baa00b483f1ce43fa8bec18c797c03")
    ;; emacs 28.2
    ;; (list "emacs" "772eaa69f31457aa19ca4dc4ce755c791d722054")
    )))

(define-public (manifest-content)
  "Leads to creation of a rather large file:
ls --human-readable --size $GUIX_EXTRA_PROFILES/emacs-29.1/manifest
564K /home/bost/.guix-extra-profiles/emacs-29.1/manifest"
  ((compose
    manifest
    (partial map to-manifest-entry)
    #;(lambda (p) (format #t "~a\n" ((@(ice-9 pretty-print) pretty-print) p)) p)
    #;(lambda (p) ((@(srfi srfi-1) take) p 2))
    (partial dbg-packages-to-install #:search-space '("emacs") #:pkgs)
    inferior-pkgs
    (partial dbg-packages-to-install #:search-space '("emacs") #:pkgs)
    (partial remove (lambda (p)
                      ;; (and (package? p) (equal? (package-name p) "emacs"))
                      (if (package? p)
                        (if (equal? (package-name p) "emacs")
                            (begin
                              (format #t "D ~a Removing ~a\n" m p)
                              #t)
                            #f)
                        #f)))
    (partial dbg-packages-to-install #:search-space '("emacs") #:pkgs))
   (packages-to-install)))
(testsymb 'manifest-content)

(module-evaluated)

(manifest-content)
