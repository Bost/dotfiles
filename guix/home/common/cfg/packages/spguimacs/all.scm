(define-module (cfg packages spguimacs all)
  #:use-module (utils) ;; partial m s+ s- sx
  ;; first take remove delete-duplicates append-map etc.
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages) ;; specification->package+output
  #:use-module ((bost gnu packages emacs-xyz) #:prefix bst:)
  #:use-module ((gnu packages emacs-xyz) #:prefix gnu:)
  #:use-module (cfg packages spguimacs needed)
  #:use-module (cfg packages spguimacs available)
  )

(evaluating-module)

(define (spacemacs-development-packages)
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

   "openssh"
   "ripgrep"
   "rsync"
   "sed"
   "which"
   ))
(testsymb 'spacemacs-development-packages)

(define (general-packages)
  (list
   "emacs-spacemacs"
   "spacemacs-rolling-release"
   ))
(testsymb 'general-packages)

(define (excluded-packages)
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
(testsymb 'excluded-packages)

;; Orphan packages according to spguimacs
(define (orphan-packages)
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

#|
(define G (general-packages))
(define N (needed-packages))
(define O (orphan-packages))
(define A (available-packages))
(define E (excluded-packages))
(load "/home/bost/dev/dotfiles/guix/home/cfg/packages/spguimacs/all.scm")
|#
(define-public (spguimacs-packages)
  (let [
        (G (general-packages))
        (N (needed-packages))
        (O (orphan-packages))
        (A (available-packages))
        (E (excluded-packages))

;;; The 'specification->package+output' can be reliably called only over
;;; available-packages since e.g. needed-packages may contain a non-existing
;;; package, i.e. a package which hasn't been ported to Guix yet.
        ;; (G (map (comp list specification->package+output) (general-packages)))
        ;; (N (map (comp list specification->package+output) (needed-packages)))
        ;; (O (map (comp list specification->package+output) (orphan-packages)))
        ;; (A (map (comp list specification->package+output) (available-packages)))
        ;; (E (map (comp list specification->package+output) (excluded-packages)))
        ]
    ((comp
      (partial
       append
       (list
        bst:emacs-lsp-ui
        bst:emacs-dap-launch
        bst:emacs-dap-tasks
        bst:emacs-lsp-docker
        bst:emacs-dap-mode
        bst:emacs-dap-utils
        bst:emacs-dap-chrome
        bst:emacs-dap-overlays
        bst:emacs-lsp-mode
        bst:emacs-helm-lsp
        bst:emacs-lsp-lens
        bst:emacs-lsp-protocol
        bst:emacs-lsp-java
        bst:emacs-lsp-metals
        bst:emacs-lsp-metals-protocol
        bst:emacs-lsp-metals-treeview
        bst:emacs-treemacs-treelib
        bst:emacs-color-theme-sanityinc-tomorrow
        bst:emacs-color-theme-sanityinc-solarized
        bst:emacs-lsp-python-ms
        bst:emacs-moe-theme
        bst:emacs-slim-mode
        bst:emacs-font-utils
        bst:emacs-lush-theme
        bst:emacs-vi-tilde-fringe
        bst:emacs-popwin
        bst:emacs-lsp-volar
        bst:emacs-centered-cursor-mode
        bst:emacs-company-statistics
        bst:emacs-json-navigator
        bst:emacs-eziam-themes
        bst:emacs-helm-cider-history
        bst:emacs-flx
        bst:emacs-lsp-haskell
        bst:emacs-helm-css-scss
        bst:emacs-auto-yasnippet
        bst:emacs-composer
        bst:emacs-erc-social-graph
        bst:emacs-hlint-refactor
        bst:emacs-chocolate
        bst:emacs-gruber-darker
        bst:emacs-writeroom-mode
        bst:emacs-js2-refactor
        bst:emacs-origami
        bst:emacs-farmhouse-themes
        bst:emacs-farmhouse-light-mod-theme
        bst:emacs-web-completion-data
        bst:emacs-company-web
        bst:emacs-gptel
        bst:emacs-copilot
        bst:emacs-ample-zen-theme
        bst:emacs-anti-zenburn-theme
        bst:emacs-apropospriate-theme
        bst:emacs-badwolf-theme
        bst:emacs-birds-of-paradise-plus-theme
        bst:emacs-bubbleberry-theme
        bst:emacs-busybee-theme
        bst:emacs-cherry-blossom-theme
        bst:emacs-clues-theme
        bst:emacs-dakrone-theme
        bst:emacs-darkmine-theme
        bst:emacs-darkokai-theme
        bst:emacs-darktooth-theme
        bst:emacs-django-theme
        bst:emacs-espresso-theme
        bst:emacs-flatland-theme
        bst:emacs-flatui-theme
        bst:emacs-gandalf-theme
        bst:emacs-gotham-theme
        bst:emacs-grandshell-theme
        bst:emacs-gruber-darker-theme
        bst:emacs-hc-zenburn-theme
        bst:emacs-hemisu-theme
        bst:emacs-heroku-theme
        bst:emacs-inkpot-theme
        bst:emacs-ir-black-theme
        bst:emacs-jazz-theme
        bst:emacs-jbeans-theme
        bst:emacs-light-soap-theme
        bst:emacs-madhat2r-theme
        bst:emacs-majapahit-themes
        bst:emacs-material-theme
        bst:emacs-minimal-theme
        bst:emacs-color-theme
        ;; bst:emacs-molokai-theme ;; doesn't compile
        bst:emacs-monochrome-theme
        bst:emacs-mustang-theme
        bst:emacs-naquadah-theme
        bst:emacs-noctilux-theme
        bst:emacs-obsidian-theme
        bst:emacs-occidental-theme
        bst:emacs-oldlace-theme
        bst:emacs-organic-green-theme
        bst:emacs-phoenix-dark-mono-theme
        bst:emacs-phoenix-dark-pink-theme
        bst:emacs-planet-theme
        bst:emacs-professional-theme
        bst:emacs-purple-haze-theme
        bst:emacs-reverse-theme
        bst:emacs-seti-theme
        bst:emacs-smyx-theme
        bst:emacs-soft-charcoal-theme
        bst:emacs-soft-morning-theme
        bst:emacs-soft-stone-theme
        bst:emacs-subatomic256-theme
        bst:emacs-subatomic-theme
        bst:emacs-sunny-day-theme
        bst:emacs-tango-2-theme
        bst:emacs-tango-plus-theme
        bst:emacs-tangotango-theme
        bst:emacs-toxi-theme
        bst:emacs-twilight-anti-bright-theme
        bst:emacs-twilight-bright-theme
        bst:emacs-twilight-theme
        bst:emacs-ujelly-theme
        bst:emacs-underwater-theme
        bst:emacs-white-sand-theme
        bst:emacs-zen-and-art-theme
        bst:emacs-omtose-phellack-theme
        bst:emacs-unicode-fonts
        bst:emacs-sublime-themes
        bst:emacs-cider-hydra
        bst:emacs-flycheck-clj-kondo
        bst:emacs-flycheck-joker
        bst:emacs-kaocha-runner
        bst:emacs-sayid
        bst:emacs-kaolin-themes
        bst:emacs-emacsql-sqlite3
        bst:emacs-zonokai-emacs
        bst:emacs-treemacs
        bst:emacs-lsp-treemacs
        bst:emacs-kill-buffers
        bst:emacs-copy-sexp
        bst:emacs-jump-last
        bst:emacs-evil-iedit-state
        bst:emacs-tweaks
        bst:emacs-term-cursor
        bst:emacs-lsp-pyright
        bst:emacs-lsp-origami
        bst:emacs-winum
        bst:emacs-frame-fns
        bst:emacs-frame-cmds
        bst:emacs-zoom-frm

        bst:emacs-evil-collection
        bst:emacs-forge
        bst:emacs-magit
        bst:emacs-magit-annex
        bst:emacs-magit-gerrit
        bst:emacs-magit-org-todos-el
        bst:emacs-magit-svn
        bst:emacs-magit-todos
        bst:emacs-orgit
        bst:emacs-taxy
        bst:emacs-taxy-magit-section
        bst:emacs-treemacs-extra
        bst:emacs-vdiff-magit

        ))
      (partial map (comp list specification->package+output))

      (partial remove (partial string= "emacs-evil-collection"))
      (partial remove (partial string= "emacs-forge"))
      (partial remove (partial string= "emacs-magit"))
      (partial remove (partial string= "emacs-magit-annex"))
      (partial remove (partial string= "emacs-magit-gerrit"))
      (partial remove (partial string= "emacs-magit-org-todos-el"))
      (partial remove (partial string= "emacs-magit-svn"))
      (partial remove (partial string= "emacs-magit-todos"))
      (partial remove (partial string= "emacs-orgit"))
      (partial remove (partial string= "emacs-taxy"))
      (partial remove (partial string= "emacs-taxy-magit-section"))
      (partial remove (partial string= "emacs-treemacs-extra"))
      (partial remove (partial string= "emacs-vdiff-magit"))

      (partial remove (partial string= "emacs-lsp-ui"))
      (partial remove (partial string= "emacs-dap-launch"))
      (partial remove (partial string= "emacs-dap-tasks"))
      (partial remove (partial string= "emacs-lsp-docker"))
      (partial remove (partial string= "emacs-dap-mode"))
      (partial remove (partial string= "emacs-dap-utils"))
      (partial remove (partial string= "emacs-dap-chrome"))
      (partial remove (partial string= "emacs-dap-overlays"))
      (partial remove (partial string= "emacs-lsp-mode"))
      (partial remove (partial string= "emacs-helm-lsp"))
      (partial remove (partial string= "emacs-lsp-lens"))
      (partial remove (partial string= "emacs-lsp-protocol"))
      (partial remove (partial string= "emacs-lsp-java"))
      (partial remove (partial string= "emacs-lsp-metals"))
      (partial remove (partial string= "emacs-lsp-metals-protocol"))
      (partial remove (partial string= "emacs-lsp-metals-treeview"))
      (partial remove (partial string= "emacs-treemacs-treelib"))
      (partial remove (partial string= "emacs-color-theme-sanityinc-tomorrow"))
      (partial remove (partial string= "emacs-color-theme-sanityinc-solarized"))
      (partial remove (partial string= "emacs-lsp-python-ms"))
      (partial remove (partial string= "emacs-moe-theme"))
      (partial remove (partial string= "emacs-slim-mode"))
      (partial remove (partial string= "emacs-font-utils"))
      (partial remove (partial string= "emacs-lush-theme"))
      (partial remove (partial string= "emacs-vi-tilde-fringe"))
      (partial remove (partial string= "emacs-popwin"))
      (partial remove (partial string= "emacs-lsp-volar"))
      (partial remove (partial string= "emacs-centered-cursor-mode"))
      (partial remove (partial string= "emacs-company-statistics"))
      (partial remove (partial string= "emacs-json-navigator"))
      (partial remove (partial string= "emacs-eziam-themes"))
      (partial remove (partial string= "emacs-helm-cider-history"))
      (partial remove (partial string= "emacs-flx"))
      (partial remove (partial string= "emacs-lsp-haskell"))
      (partial remove (partial string= "emacs-helm-css-scss"))
      (partial remove (partial string= "emacs-auto-yasnippet"))
      (partial remove (partial string= "emacs-composer"))
      (partial remove (partial string= "emacs-erc-social-graph"))
      (partial remove (partial string= "emacs-hlint-refactor"))
      (partial remove (partial string= "emacs-chocolate"))
      (partial remove (partial string= "emacs-gruber-darker"))
      (partial remove (partial string= "emacs-writeroom-mode"))
      (partial remove (partial string= "emacs-js2-refactor"))
      (partial remove (partial string= "emacs-origami"))
      (partial remove (partial string= "emacs-farmhouse-themes"))
      (partial remove (partial string= "emacs-farmhouse-light-mod-theme"))
      (partial remove (partial string= "emacs-web-completion-data"))
      (partial remove (partial string= "emacs-company-web"))
      (partial remove (partial string= "emacs-gptel"))
      (partial remove (partial string= "emacs-copilot"))
      (partial remove (partial string= "emacs-ample-zen-theme"))
      (partial remove (partial string= "emacs-anti-zenburn-theme"))
      (partial remove (partial string= "emacs-apropospriate-theme"))
      (partial remove (partial string= "emacs-badwolf-theme"))
      (partial remove (partial string= "emacs-birds-of-paradise-plus-theme"))
      (partial remove (partial string= "emacs-bubbleberry-theme"))
      (partial remove (partial string= "emacs-busybee-theme"))
      (partial remove (partial string= "emacs-cherry-blossom-theme"))
      (partial remove (partial string= "emacs-clues-theme"))
      (partial remove (partial string= "emacs-dakrone-theme"))
      (partial remove (partial string= "emacs-darkmine-theme"))
      (partial remove (partial string= "emacs-darkokai-theme"))
      (partial remove (partial string= "emacs-darktooth-theme"))
      (partial remove (partial string= "emacs-django-theme"))
      (partial remove (partial string= "emacs-espresso-theme"))
      (partial remove (partial string= "emacs-flatland-theme"))
      (partial remove (partial string= "emacs-flatui-theme"))
      (partial remove (partial string= "emacs-gandalf-theme"))
      (partial remove (partial string= "emacs-gotham-theme"))
      (partial remove (partial string= "emacs-grandshell-theme"))
      (partial remove (partial string= "emacs-gruber-darker-theme"))
      (partial remove (partial string= "emacs-hc-zenburn-theme"))
      (partial remove (partial string= "emacs-hemisu-theme"))
      (partial remove (partial string= "emacs-heroku-theme"))
      (partial remove (partial string= "emacs-inkpot-theme"))
      (partial remove (partial string= "emacs-ir-black-theme"))
      (partial remove (partial string= "emacs-jazz-theme"))
      (partial remove (partial string= "emacs-jbeans-theme"))
      (partial remove (partial string= "emacs-light-soap-theme"))
      (partial remove (partial string= "emacs-madhat2r-theme"))
      (partial remove (partial string= "emacs-majapahit-themes"))
      (partial remove (partial string= "emacs-material-theme"))
      (partial remove (partial string= "emacs-minimal-theme"))
      (partial remove (partial string= "emacs-color-theme"))
      ;; (partial remove (partial string= "emacs-molokai-theme")) ;; doesn't compile
      (partial remove (partial string= "emacs-monochrome-theme"))
      (partial remove (partial string= "emacs-mustang-theme"))
      (partial remove (partial string= "emacs-naquadah-theme"))
      (partial remove (partial string= "emacs-noctilux-theme"))
      (partial remove (partial string= "emacs-obsidian-theme"))
      (partial remove (partial string= "emacs-occidental-theme"))
      (partial remove (partial string= "emacs-oldlace-theme"))
      (partial remove (partial string= "emacs-organic-green-theme"))
      (partial remove (partial string= "emacs-phoenix-dark-mono-theme"))
      (partial remove (partial string= "emacs-phoenix-dark-pink-theme"))
      (partial remove (partial string= "emacs-planet-theme"))
      (partial remove (partial string= "emacs-professional-theme"))
      (partial remove (partial string= "emacs-purple-haze-theme"))
      (partial remove (partial string= "emacs-reverse-theme"))
      (partial remove (partial string= "emacs-seti-theme"))
      (partial remove (partial string= "emacs-smyx-theme"))
      (partial remove (partial string= "emacs-soft-charcoal-theme"))
      (partial remove (partial string= "emacs-soft-morning-theme"))
      (partial remove (partial string= "emacs-soft-stone-theme"))
      (partial remove (partial string= "emacs-subatomic256-theme"))
      (partial remove (partial string= "emacs-subatomic-theme"))
      (partial remove (partial string= "emacs-sunny-day-theme"))
      (partial remove (partial string= "emacs-tango-2-theme"))
      (partial remove (partial string= "emacs-tango-plus-theme"))
      (partial remove (partial string= "emacs-tangotango-theme"))
      (partial remove (partial string= "emacs-toxi-theme"))
      (partial remove (partial string= "emacs-twilight-anti-bright-theme"))
      (partial remove (partial string= "emacs-twilight-bright-theme"))
      (partial remove (partial string= "emacs-twilight-theme"))
      (partial remove (partial string= "emacs-ujelly-theme"))
      (partial remove (partial string= "emacs-underwater-theme"))
      (partial remove (partial string= "emacs-white-sand-theme"))
      (partial remove (partial string= "emacs-zen-and-art-theme"))
      (partial remove (partial string= "emacs-omtose-phellack-theme"))
      (partial remove (partial string= "emacs-unicode-fonts"))
      (partial remove (partial string= "emacs-sublime-themes"))
      (partial remove (partial string= "emacs-cider-hydra"))
      (partial remove (partial string= "emacs-flycheck-clj-kondo"))
      (partial remove (partial string= "emacs-flycheck-joker"))
      (partial remove (partial string= "emacs-kaocha-runner"))
      (partial remove (partial string= "emacs-sayid"))
      (partial remove (partial string= "emacs-kaolin-themes"))
      (partial remove (partial string= "emacs-emacsql-sqlite3"))
      (partial remove (partial string= "emacs-zonokai-emacs"))
      (partial remove (partial string= "emacs-treemacs"))
      (partial remove (partial string= "emacs-lsp-treemacs"))
      (partial remove (partial string= "emacs-kill-buffers"))
      (partial remove (partial string= "emacs-copy-sexp"))
      (partial remove (partial string= "emacs-jump-last"))
      (partial remove (partial string= "emacs-evil-iedit-state"))
      (partial remove (partial string= "emacs-tweaks"))
      (partial remove (partial string= "emacs-term-cursor"))
      (partial remove (partial string= "emacs-lsp-pyright"))
      (partial remove (partial string= "emacs-lsp-origami"))
      (partial remove (partial string= "emacs-winum"))
      (partial remove (partial string= "emacs-frame-fns"))
      (partial remove (partial string= "emacs-frame-cmds"))
      (partial remove (partial string= "emacs-zoom-frm"))
      )
     (s+ G
         (s- (sx (s+ N O)
                 A)
             E)))))
(testsymb 'spguimacs-packages)

(module-evaluated)

#;(specifications->manifest (spguimacs-packages))
