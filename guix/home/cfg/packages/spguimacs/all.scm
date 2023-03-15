(define-module (cfg packages spguimacs all)
  #:use-module (srfi srfi-1)
  #:use-module (utils) ;; partial, m
  #:use-module (cfg packages spguimacs needed)
  #:use-module (cfg packages spguimacs available)
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
(testsymb 'spacemacs-development-packages)

(define general-packages
  (list
   "emacs"
   "emacs-spacemacs"
   "spacemacs-rolling-release"
   ))
(testsymb 'general-packages)


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

   ;; beg: Temporarily exclude following packages. They are added in the home-config-ecke.scm
   "emacs-ac-ispell"
   "emacs-ac-php"
   "emacs-ace-jump-helm-line"
   "emacs-afternoon-theme"
   "emacs-cfrs"
   "emacs-cider-eval-sexp-fu"
   "emacs-font-lock+"
   "emacs-xcscope"
   ;; end: Temporarily exclude following packages. They are added in the home-config-ecke.scm

   "emacs-auto-yasnippet"
   "emacs-clojure-snippets"

;;; Show inlined images (png/jpg/gif/svg) in ERC buffers.
;;; https://github.com/kidd/erc-image.el Not really needed.
;;; Throws:
;;; Error loading autoloads: (file-missing Cannot open load file No such file or directory /gnu/store/w29gvdsv26r5minwgdmb1pq4dzgbi959-emacs-erc-image-0-3.82fb387/share/emacs/site-lisp/erc-image-0-3.82fb387/erc-image-autoloads)
   "emacs-erc-image"

;;; Show '<current match> / <total matches>' in the mode-line.
;;; https://github.com/syohex/emacs-evil-anzu . Doesn't work with
;;; (spaceline-all-the-icons-theme) enabled. Would be really nice to have, and
;;; nice to have it working even under the spaceline-all-the-icons-theme.
;;; Throws:
;;; Error loading autoloads: (file-missing Cannot open load file No such
;;; file or directory
;;; /gnu/store/wfy7v1jm0hjhv8cjxgi5asfcn5ma6795-emacs-evil-anzu-0.03/share/emacs/site-lisp/evil-anzu-0.03/evil-anzu-autoloads)
   "emacs-evil-anzu"

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

   "emacs-haskell-snippets"
   "emacs-helm-c-yasnippet"

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

;;; TODO test use-package
   "emacs-use-package"

;;; Distraction-free writing for Emacs.
;;; https://github.com/joostkremers/writeroom-mode emacs-writeroom corresponds
;;; to writeroom-mode, however it's ignored by Spacemacs. Probably due to the
;;; missing '-mode'.
;;; Not really needed
   "emacs-writeroom"

   "emacs-yasnippet"
   "emacs-yasnippet-snippets"
   ))
(testsymb 'excluded-packages)

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
(testsymb 'orphan-packages)

;; neither `=' nor `eqv?' work
(define eq-op? string-ci=?)

(define (s+ . rest) (apply (partial lset-union eq-op?) rest))
(testsymb 's+)
(define (s- . rest) (apply (partial lset-difference eq-op?) rest))
(testsymb 's-)
(define (sx . rest) (apply (partial lset-intersection eq-op?) rest))
(testsymb 'sx)

#|
(define G general-packages)
(define N needed-packages)
(define O orphan-packages)
(define A available-packages)
(define E excluded-packages)
(load "/home/bost/dev/dotfiles/guix/home/cfg/packages/spguimacs/all.scm")
|#
(define (spguimacs-packages)
  (let [(G general-packages)
        (N needed-packages)
        (O orphan-packages)
        (A available-packages)
        (E excluded-packages)]
    (s+ G
        (s- (sx (s+ N O)
                A)
            E))))
(testsymb 'spguimacs-packages)

;; (format #t "~a module evaluated\n" m)

#;(specifications->manifest spguimacs-packages)