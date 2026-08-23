#!/usr/bin/env -S guile \\
-L /home/bost/dev/bstx/src -L /home/bost/dev/dotfiles/guix/common -L /home/bost/dev/dotfiles/guix/home/common -e (create-elisp-package-definition) -s
!#

(define-module (create-elisp-package-definition)
  #:use-module (ice-9 match)
  #:use-module (bost common utils)
  #:use-module (scm-bin git-clone)
  #:use-module (srfi srfi-1)        ; list-processing procedures (remove)
  #:export (main)
  )

(map load
     (list
      "/home/bost/dev/dotfiles/analyzed.scm"
      ))

;; (define url "https://github.com/Bost/search-notes")
;; (define dst-dir "/tmp/search-notes")

(define* (latest-commit-hash dst-dir #:key (verbose #t))
  "Get the latest commit hash from a repository in the DST-DIR."
  ((compose
    cadr
    (lambda (prm) (exec prm #:verbose verbose))
    (partial format #f "git --git-dir=~a/.git log -n 1 --format=%H"))
   dst-dir))

(define* (latest-base32 dst-dir #:key (verbose #t))
  "Get the latest commit hash from a repository in the DST-DIR."
  ((compose
    cadr
    (lambda (prm) (exec prm #:verbose verbose))
    (partial format #f "guix hash -x --serializer=nar ~a"))
   dst-dir))

(define (git-clone-to-tmp url)
  "Clone the git repository at the URL to the /tmp/
See also
`guix download ${recurse} --commit=${url.commit} ${url.url}`
"
  (let* [(project-name (basename url))
         (dst-dir (str "/tmp/" project-name))]
    ;; (format #t "(access? dst-dir F_OK): ~a\n" (access? dst-dir F_OK)
    (if (access? dst-dir F_OK)
        dst-dir
        (git-clone url dst-dir))))

;; (access? some-file F_OK) ;; check if file exists
;; (access? some-file W_OK) ;; check if file is writable
;;   file-is-directory? (? file-exists?)

(define (make-pkg pkg)
  (format #t "pkg : ~a\n" pkg)
  ;; (format #t "url : ~a\n" (plist-get pkg 'repo-url))
  (format #t "url : ~a\n" (cadr pkg))
  (format #t "version : ~a\n" (caddr pkg))
  ;; (let* [
  ;;        (emacs-pkg-name (plist-get pkg    'emacs-pkg-name))
  ;;        (local-repo (plist-get pkg        'local-repo))
  ;;        (guix-package (plist-get pkg      'guix-package))
  ;;        (repo-url (plist-get pkg          'repo-url))
  ;;        (version (plist-get pkg           'version))
  ;;        (propagated-inputs (plist-get pkg 'propagated-inputs))
  ;;        (file (plist-get pkg              'file))
  ;;        ]
  ;;   (format #t "emacs-pkg-name : ~a\n" emacs-pkg-name)
  ;;   (format #t "local-repo : ~a\n" local-repo)
  ;;   (format #t "guix-package : ~a\n" guix-package)
  ;;   (format #t "repo-url : ~a\n" repo-url)
  ;;   (format #t "version : ~a\n" version)
  ;;   (format #t "propagated-inputs : ~a\n" propagated-inputs)
  ;;   (format #t "file : ~a\n" file))

  (let* [(url
          (cadr pkg)
          ;; (plist-get pkg 'repo-url)
          )
         (dst-dir (git-clone-to-tmp url))]
    ;; (format #t "[make-pkg] dst-dir ~a\n" dst-dir)
    ;; (format #t "[make-pkg] (basename dst-dir) ~a\n" (basename dst-dir))
    (when dst-dir
      (let* [
             ;; (emacs-pkg-name (str (car pkg) "-theme"))
             (emacs-pkg-name (car pkg))
             ;; (emacs-pkg-name (plist-get pkg 'emacs-pkg-name))
             ]
        (format #t "emacs-pkg-name : ~a\n" emacs-pkg-name)
        (let* [
               (verbose               #f)
               (version
                ;; (plist-get pkg 'version)
                (caddr pkg)
                )
               (propagated-inputs
                (cadddr pkg)
                ;; (plist-get pkg 'propagated-inputs)
                )
               (emacs-pkg-name-symbol (string->symbol emacs-pkg-name))
               (commit                (latest-commit-hash dst-dir #:verbose verbose))

               (package-specification
                (remove unspecified?
                        (list
                         `(name ,emacs-pkg-name)
                         `(version (git-version ,version revision commit))
                         `(source
                           (origin
                             (method git-fetch)
                             (uri (git-reference
                                   (url ,url)
                                   (commit commit)))
                             (file-name (git-file-name name version))
                             (sha256
                              (base32 ,(latest-base32 dst-dir #:verbose verbose)))))
                         `(build-system emacs-build-system)
                         (unless (equal? '(propagated-inputs (list))
                                         propagated-inputs)
                           propagated-inputs)
                         `(home-page ,url)
                         `(synopsis "") ;; TODO synopsis
                         `(description "") ;; TODO description
                         `(license license:gpl3+))))
               ]

          ;; (format #t "(equal? '(propagated-inputs (list)) propagated-inputs): ~a\n"
          ;;         (equal? '(propagated-inputs (list)) propagated-inputs))
          ;; (format #t "package-specification: ~a\n" package-specification)

          (pretty-print->string
           `(define-public ,emacs-pkg-name-symbol
              (let ((commit ,commit)
                    (revision "0"))
                (package ,@package-specification)))))))))

;; (define (prepare pkg)
;;   (pretty-print->string
;;    `(#:url ,(car pkg)
;;      #:version ,(cadr pkg)
;;      #:file ,(caddr pkg)
;;      #:package-name "TODO get from the repository under the #:url; analyze content of the .el files; the '(provides ...)'"
;;      #:requires "TODO get from the repository under the #:url; analyze content of the .el files; the '(require ...)'"
;;      #:synopsis "TODO get from the repository under the #:url"
;;      #:description "TODO get from the repository under the #:url"
;;      #:license "TODO get from the repository under the #:url. It must be one of the licenses defined in the 'guix/guix/licenses.scm' from the source code of the GNU Guix project"
;;      )))

;; (git-clone-to-tmp "https://github.com/purcell/color-theme-sanityinc-tomorrow")

(define* (main #:rest args)
  (map (compose
        (partial format #t "\n~a\n")
        make-pkg)
       pkgs-analyzed
       #;
       (list (car pkgs-analyzed) (cadr pkgs-analyzed) (caddr pkgs-analyzed))))
(testsymb 'main)

#;
(load "/home/bost/dev/dotfiles/create-elisp-package-definition.scm")
