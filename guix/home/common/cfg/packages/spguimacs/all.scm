(define-module (cfg packages spguimacs all)
  #:use-module (utils) ;; partial m s+ s- sx
  ;; first take remove delete-duplicates append-map etc.
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages) ;; specification->package+output
  #:use-module ((bost gnu packages emacs-xyz) #:prefix bst:)
  #:use-module ((gnu packages emacs-xyz) #:prefix gnu:)
  #:use-module (cfg packages spguimacs needed)
  #:use-module (cfg packages spguimacs available)
  #:use-module (guix) ;; package-name
  )

(evaluating-module)

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

   ;; Show inlined images (png/jpg/gif/svg) in ERC buffers.
   ;; https://github.com/kidd/erc-image.el
   "emacs-erc-image"

   ;; Preview candidates when using Evil registers and marks.
   ;; https://github.com/mamapanda/evil-owl
   "emacs-evil-owl" ;; seems not to be used

   ;; Display small plots and graphics and lets users write shell commands in
   ;; Emacs Lisp. https://github.com/riscy/shx-for-emacs
   "emacs-shx"
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

(define bst-packages
  (list
   bst:emacs-vim-powerline-theme
   bst:emacs-scss-mode
   bst:emacs-helm-pydoc
   bst:emacs-php-auto-yasnippets
   bst:emacs-js-doc
   bst:emacs-multi-line

   bst:emacs-helm-dictionary
   bst:emacs-pcache
   bst:emacs-groovy-imports

   ;; bst:emacs-ac-php-core
   bst:emacs-ample-zen-theme
   bst:emacs-anti-zenburn-theme
   bst:emacs-apropospriate-theme
   bst:emacs-badwolf-theme
   bst:emacs-birds-of-paradise-plus-theme
   bst:emacs-bubbleberry-theme
   bst:emacs-busybee-theme
   bst:emacs-centered-cursor-mode
   bst:emacs-chatgpt
   bst:emacs-chatgpt-shell
   bst:emacs-cherry-blossom-theme
   bst:emacs-chocolate
   bst:emacs-cider-hydra
   bst:emacs-clues-theme
   bst:emacs-color-theme
   bst:emacs-color-theme-sanityinc-solarized
   bst:emacs-color-theme-sanityinc-tomorrow
   bst:emacs-company-anaconda
   bst:emacs-company-php
   bst:emacs-company-phpactor
   bst:emacs-company-statistics
   bst:emacs-company-web
   bst:emacs-copilot
   bst:emacs-copy-sexp
   bst:emacs-dakrone-theme
   bst:emacs-dap-chrome
   bst:emacs-dap-launch
   bst:emacs-dap-overlays
   bst:emacs-dap-tasks
   bst:emacs-dap-utils
   bst:emacs-darkmine-theme
   bst:emacs-darkokai-theme
   bst:emacs-darktooth-theme
   bst:emacs-django-theme
   bst:emacs-emacsql-sqlite3
   bst:emacs-erc-social-graph
   bst:emacs-espresso-theme
   bst:emacs-evil-collection
   bst:emacs-evil-iedit-state
   bst:emacs-eziam-themes
   bst:emacs-farmhouse-light-mod-theme
   bst:emacs-farmhouse-themes
   bst:emacs-flatland-theme
   bst:emacs-flatui-theme
   bst:emacs-flx
   bst:emacs-flycheck-joker
   bst:emacs-font-utils
   bst:emacs-frame-cmds
   bst:emacs-frame-fns
   bst:emacs-gandalf-theme
   bst:emacs-git-commit
   bst:emacs-gotham-theme
   bst:emacs-grandshell-theme
   bst:emacs-gruber-darker
   bst:emacs-gruber-darker-theme
   bst:emacs-hc-zenburn-theme
   bst:emacs-helm-cider-history
   bst:emacs-helm-core
   bst:emacs-helm-files
   bst:emacs-helm-lsp
   bst:emacs-hemisu-theme
   bst:emacs-heroku-theme
   bst:emacs-hlint-refactor
   bst:emacs-inkpot-theme
   bst:emacs-ir-black-theme
   bst:emacs-jazz-theme
   bst:emacs-jbeans-theme
   bst:emacs-js2-refactor
   bst:emacs-json-navigator
   bst:emacs-jump-last
   bst:emacs-kaocha-runner
   bst:emacs-kaolin-themes
   bst:emacs-kill-buffers
   bst:emacs-light-soap-theme
   bst:emacs-lsp-haskell
   bst:emacs-lsp-java
   bst:emacs-lsp-lens
   bst:emacs-lsp-metals
   bst:emacs-lsp-metals-protocol
   bst:emacs-lsp-metals-treeview
   bst:emacs-lsp-origami
   bst:emacs-lsp-protocol
   bst:emacs-lsp-pyright
   bst:emacs-lsp-python-ms
   bst:emacs-lsp-volar
   bst:emacs-lush-theme
   bst:emacs-lv
   bst:emacs-madhat2r-theme
   bst:emacs-magit-gerrit
   bst:emacs-magit-org-todos-el
   bst:emacs-magit-section
   bst:emacs-magit-svn
   bst:emacs-majapahit-themes
   bst:emacs-markdown-toc
   bst:emacs-material-theme
   bst:emacs-minimal-theme
   bst:emacs-moe-theme
   ;; bst:emacs-molokai-theme ;; doesn't compile
   bst:emacs-monochrome-theme
   bst:emacs-mustang-theme
   bst:emacs-naquadah-theme
   bst:emacs-noctilux-theme
   bst:emacs-obsidian-theme
   bst:emacs-occidental-theme
   bst:emacs-oldlace-theme

;;; bst:emacs-omtose-phellack-theme produces:
;;;    Error loading autoloads: (file-missing Cannot open load file No such file or directory /gnu/store/...-emacs-omtose-phellack-theme-0.2.0-0.66f9963/share/emacs/site-lisp/omtose-phellack-theme-0.2.0-0.66f9963/omtose-phellack-theme-autoloads)
;;; but this doesn't stop spacemacs from running.
   bst:emacs-omtose-phellack-theme

   bst:emacs-openai
   bst:emacs-organic-green-theme
   bst:emacs-origami
   bst:emacs-phoenix-dark-mono-theme
   bst:emacs-phoenix-dark-pink-theme
   bst:emacs-php-runtime
   bst:emacs-phpactor
   bst:emacs-planet-theme
   bst:emacs-popwin
   bst:emacs-professional-theme
   bst:emacs-purple-haze-theme
   bst:emacs-reverse-theme
   bst:emacs-sayid
   bst:emacs-seti-theme
   bst:emacs-slim-mode
   bst:emacs-smyx-theme
   bst:emacs-soft-charcoal-theme
   bst:emacs-soft-morning-theme
   bst:emacs-soft-stone-theme
   bst:emacs-subatomic-theme
   bst:emacs-subatomic256-theme
   bst:emacs-sublime-themes
   bst:emacs-sunny-day-theme
   bst:emacs-tango-2-theme
   bst:emacs-tango-plus-theme
   bst:emacs-tangotango-theme
   bst:emacs-tblui
   bst:emacs-term-cursor
   bst:emacs-toxi-theme
   bst:emacs-treemacs-extra
   bst:emacs-treemacs-magit
   bst:emacs-treemacs-treelib
   bst:emacs-tweaks
   bst:emacs-twilight-anti-bright-theme
   bst:emacs-twilight-bright-theme
   bst:emacs-twilight-theme
   bst:emacs-ujelly-theme
   bst:emacs-underwater-theme
   bst:emacs-unicode-fonts
   bst:emacs-use-package
   bst:emacs-use-package-chords
   bst:emacs-vdiff-magit
   bst:emacs-vi-tilde-fringe
   bst:emacs-web-completion-data
   bst:emacs-white-sand-theme
   bst:emacs-winum
   bst:emacs-writeroom-mode
   bst:emacs-zen-and-art-theme
   bst:emacs-zonokai-emacs
   bst:emacs-zoom-frm
   ))
(testsymb-trace 'bst-packages)

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
      (partial append bst-packages)
      (partial map (comp list specification->package+output))
      (lambda (lst) (s- lst (map package-name bst-packages))))
     (s+ G
         (s- (sx (s+ N O)
                 A)
             E)))))
(testsymb 'spguimacs-packages)

(module-evaluated)

#;(specifications->manifest (spguimacs-packages))
