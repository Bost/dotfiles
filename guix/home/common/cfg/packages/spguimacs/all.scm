(define-module (cfg packages spguimacs all)
  #:use-module (utils) ;; partial m s+ s- sx
  #:use-module (gnu packages) ;; specification->package+output
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
  ;;   ("/home/bost/.emacs.d.spacemacs/snippets")

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
  (let [
        (G general-packages)
        (N needed-packages)
        (O orphan-packages)
        (A available-packages)
        (E excluded-packages)

;;; The 'specification->package+output' can be reliably called only over
;;; available-packages since e.g. needed-packages may contain a non-existing
;;; package, i.e. a package which hasn't been ported to Guix yet.
        ;; (G (map (comp list specification->package+output) general-packages))
        ;; (N (map (comp list specification->package+output) needed-packages))
        ;; (O (map (comp list specification->package+output) orphan-packages))
        ;; (A (map (comp list specification->package+output) available-packages))
        ;; (E (map (comp list specification->package+output) excluded-packages))
        ]
    ((comp 
      (partial map (comp list specification->package+output)))
     (s+ G
         (s- (sx (s+ N O)
                 A)
             E)))))

;; (format #t "~a module evaluated\n" m)

#;(specifications->manifest spguimacs-packages)
