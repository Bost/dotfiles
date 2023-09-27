(define-module (cfg packages spguimacs all)
  #:use-module (utils) ;; partial m s+ s- sx
  #:use-module (cfg packages spguimacs needed)
  #:use-module (cfg packages spguimacs available)

  ;; the code for this module comes from the 'bost' channel. See
  ;; ~/.config/guix/channels.scm
  #:use-module ((bost packages emacs-xyz) #:prefix bste:)

  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  ;; first take remove delete-duplicates append-map etc.
  #:use-module (srfi srfi-1)

  #:export (
            spguimacs-packages
            ))

(define m (module-name-for-logging))
;; (format #t "~a evaluating module ...\n" m)

(define spacemacs-development-packages
  ;; used by `guix shell ...', specified by run.sh
  (list
   "bash"

   "coreutils"
   "curl"

   ;; provides:
   ;; find, updatedb, xargs
   "findutils"

   ;; provides:
   ;; free, pgrep, pidof, pkill, pmap, ps, pwdx, slabtop, tload, top, vmstat,
   ;; w, watch and sysctl
   ;; `w' - Show who is logged on and what they are doing.
   "procps"
   "fish"
   "git"
   "gnupg"
   "grep"
   "less"
   "ncurses"
   "nss-certs"

   "openssh"
   "ripgrep"
   "rsync"
   "sed"
   "which"
   ))

(define general-packages
  (list
   "emacs-spacemacs"
   "mu" ;; for mu4e, which is for treemacs-mu4e
   "spacemacs-rolling-release"
   ))

(define excluded-packages
  ;; (ya)snippet-relates packages cause adding various paths to `yas-snippet-dirs',
  ;; among others the <current-dir>/snippets which cannot be opened.
  ;; The error is, e.g.:
  ;;     Error (use-package): cider/:catch: Opening directory: No such file or directory, <current-dir>/snippets
  ;;     Opening directory: No such file or directory, <current-dir>/snippets
  ;;
  ;; `yas-snippet-dirs' value is, e.g.:
  ;;   ("/home/bost/.spacemacs-guix.d/snippets"
  ;;    "/home/bost/.spacemacs-guix.d/private/snippets/"
  ;;    "/home/bost/.spacemacs-guix.d/layers/+completion/auto-completion/local/snippets"
  ;;    "/gnu/store/lm1pa3z1aafp62fw6vf5fwfbic7wf6yw-emacs-clojure-snippets-1.0.1-0.6068dca/share/emacs/site-lisp/clojure-snippets-1.0.1-0.6068dca/snippets"
  ;;    "/home/bost/snippets")
  ;; The original value was:
  ;;   ("/home/bost/.emacs.d/snippets")

  (list
   ;; beg: Temporarily exclude following packages. They are added in the home-<hostname>.scm
   ;; end: Temporarily exclude following packages. They are added in the home-<hostname>.scm

;;; Show inlined images (png/jpg/gif/svg) in ERC buffers.
;;; https://github.com/kidd/erc-image.el Not really needed.
;;; Throws:
;;; Error loading autoloads: (file-missing Cannot open load file No such file or directory /gnu/store/w29gvdsv26r5minwgdmb1pq4dzgbi959-emacs-erc-image-0-3.82fb387/share/emacs/site-lisp/erc-image-0-3.82fb387/erc-image-autoloads)
   "emacs-erc-image"

;;; Preview candidates when using Evil registers and marks.
;;; https://github.com/mamapanda/evil-owl
;;; Seems not to be used anyway
;;; "compilation" problem
   "emacs-evil-owl"

;;; Edit Gherkin / Cucumber plain text user stories. See examples
;;; https://cucumber.io/docs/gherkin/reference/
;;; Not needed.
;;; broken! spacemacs doesn't start: invalid version 0.5.0-dev
   "emacs-feature-mode"

;;; Extras for the comint-mode shell. https://github.com/riscy/shx-for-emacs
;;; Would be nice to have.
;;; "compilation" problem
   "emacs-shx"

;;; Emacs leader key implementation from Spacemacs. Should be excluded anyway.
;;; Throws:
;;; Error (use-package): Failed to parse package column-enforce-mode: use-package: Unrecognized keyword: :spacediminish Disable showing Disable logging
;;; Error (use-package): Failed to parse package highlight-indentation: use-package: Unrecognized keyword: :spacediminish Disable showing Disable logging
;;; Error (use-package): Failed to parse package indent-guide: use-package: Unrecognized keyword: :spacediminish Disable showing Disable logging
;;; Error (use-package): Failed to parse package fill-column-indicator: use-package: Unrecognized keyword: :spacediminish Disable showing Disable logging
   "emacs-spaceleader"
   ))

;; Orphan packages according to spguimacs
(define orphan-packages
  (list
   "emacs-faceup"
   "emacs-deferred"
   "emacs-undercover"
   "emacs-treeview" ;; installed by emacs-inspector
   "emacs-pg"
   "emacs-finalize"
   "emacs-ivy"
   "emacs-a"
   ))

#|
(define G general-packages)
(define N needed-packages)
(define O orphan-packages)
(define A available-packages)
(define E excluded-packages)
(load "/home/bost/dev/dotfiles/guix/home/cfg/packages/spguimacs/all.scm")
|#

(define (spguimacs-packages)
  ((compose
    ;; (lambda (pkgs)
    ;;   ;;
    ;;   (format #t "~a\n~a\n" m
    ;;           (let* [(search-space
    ;;                   '(
    ;;                     "emacs-haskell-snippets"
    ;;                     "emacs-yasnippet"
    ;;                     "emacs-yasnippet-snippets"
    ;;                     ))]
    ;;             ((compose
    ;;               (partial
    ;;                filter
    ;;                ;; see also `string=?', `eq?', `equal?', etc.
    ;;                ;; member uses `equal?'
    ;;                (lambda (p)
    ;;                  (cond
    ;;                   [(list? p)    (member (package-name (car p))
    ;;                                         search-space)]
    ;;                   [(string? p)  (member (package-name p) search-space)]
    ;;                   [(package? p) (member (package-name p) search-space)]
    ;;                   [(record? p)  (member (inferior-package-name p)
    ;;                                         search-space)]
    ;;                   [else         (member (package-name p) search-space)])))
    ;;               ;; we have a colorful mix here:
    ;;               (partial
    ;;                map
    ;;                (lambda (p)
    ;;                  (cond
    ;;                   [(list? p)    (when (member (package-name (car p))
    ;;                                               search-space)
    ;;                                   (format #t "list    ~a\n" p))]
    ;;                   [(string? p)  (when #t
    ;;                                   (format #t "string  ~a\n" p))]
    ;;                   [(package? p) (when #t
    ;;                                   (format #t "package ~a\n" p))]
    ;;                   [(record? p)  (when #t
    ;;                                   (format #t "record  ~a\n" p))]
    ;;                   [else         (when #t
    ;;                                   (format #t "else    ~a\n" p))])
    ;;                  p))
    ;;               identity)
    ;;              pkgs)))
    ;;   ;;
    ;;   ;; (format #t "~a\n" (string-join (map (partial format #f "~a") pkgs) "\n"))
    ;;   (format #t "~a packages to install: ~a\n" m (length pkgs))
    ;;   pkgs)

    ;; (partial append (list
    ;;                  ;; bste:emacs-copilot
    ;;                  ;; below are good
    ;;                  ;; bste:emacs-lua-mode
    ;;                  bste:emacs-emacsql
    ;;                  bste:emacs-closql
    ;;                  bste:emacs-forge
    ;;                  ;; bste:emacs-emacsql-sqlite3
    ;;                  bste:emacs-company-web
    ;;                  bste:emacs-web-completion-data
    ;;                  bste:emacs-centered-cursor-mode
    ;;                  bste:emacs-company-statistics
    ;;                  bste:emacs-json-navigator
    ;;                  bste:emacs-eziam-themes
    ;;                  bste:emacs-helm-cider-history
    ;;                  bste:emacs-lsp-haskell
    ;;                  bste:emacs-helm-css-scss
    ;;                  ;; bste:emacs-auto-yasnippet
    ;;                  bste:emacs-composer
    ;;                  bste:emacs-erc-social-graph
    ;;                  bste:emacs-chocolate
    ;;                  bste:emacs-gruber-darker

    ;;                  bste:emacs-vi-tilde-fringe
    ;;                  bste:emacs-popwin
    ;;                  ;; bste:emacs-paradox
    ;;                  bste:emacs-lsp-volar

    ;;                  bste:emacs-lsp-python-ms
    ;;                  bste:emacs-slim-mode
    ;;                  bste:emacs-zop-to-char
    ;;                  bste:emacs-font-utils
    ;;                  ;; bste:emacs-pythonic

    ;;                  bste:emacs-lsp-metals
    ;;                  bste:emacs-lsp-java
    ;;                  bste:emacs-dap-mode
    ;;                  bste:emacs-lsp-treemacs
    ;;                  bste:emacs-treemacs
    ;;                  ))

    (partial map (compose identity list specification->package))

    ;; (lambda (v) (format #t "1\n~a\n" v) v)
    ;; TODO `eq?' works for "lua" but not for "emacs-popwin". WTF!?
    (partial remove (partial string= "emacs-popwin"))
    ;; (lambda (v) (map (lambda (p) (format #t "~a ~a\n" p (string? p))) v) v)
    ;; (lambda (v) (format #t "0\n~a\n" v) v)
    )
   (let [(G general-packages)
         (N needed-packages)
         (O orphan-packages)
         (A available-packages)
         (E excluded-packages)]
     (s+ G
         (s- (sx (s+ N O)
                 A)
             E)))))
(testsymb 'spguimacs-packages)

;; (format #t "~a module evaluated\n" m)
