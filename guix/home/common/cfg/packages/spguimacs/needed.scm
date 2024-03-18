(define-module (cfg packages spguimacs needed)
  #:use-module (srfi srfi-1)
  #:use-module (utils) ;; partial, m
)

(evaluating-module)

;; Specifying package using @, ie. "(@ (gnu packages emacs-xyz) emacs-guix)"
;; doesn't work
(define-public (needed-packages)
  (list
   ;; :beg: not really needed although listed in the $dev/.spguimacs.d/layers/+lang/clojure/packages.el
   ;; "emacs-flycheck-joker"
   ;; "emacs-kaocha-runner"
   ;; :end: not really needed although listed in the $dev/.spguimacs.d/layers/+lang/clojure/packages.el

   ;; "emacs-edbi-sqlite"

   ;; "emacs-hybrid-mode"  ;; part of spacemacs

   ;; "emacs-molokai-theme"  ;; needs emacs-color-theme ;; doesn't compile
   "emacs-ac-ispell"
   "emacs-ac-php"
   "emacs-ace-jump-helm-line"
   "emacs-ace-link"
   "emacs-ace-window"
   "emacs-afternoon-theme"
   "emacs-aggressive-indent"
   "emacs-alect-themes"
   "emacs-alert"
   "emacs-all-the-icons"
   "emacs-ample-theme"
   "emacs-ample-zen-theme"
   "emacs-anaconda-mode"
   "emacs-annalist"
   "emacs-anti-zenburn-theme"
   "emacs-anzu"
   "emacs-apropospriate-theme"
   "emacs-async"
   "emacs-attrap"
   "emacs-auctex"
   "emacs-auto-compile"
   "emacs-auto-complete"
   "emacs-auto-dictionary"
   "emacs-auto-highlight-symbol"
   "emacs-autothemer"
   "emacs-avy"
   "emacs-badwolf-theme"
   "emacs-beacon"
   "emacs-bind-key"
   "emacs-bind-map"
   "emacs-birds-of-paradise-plus-theme"
   "emacs-blacken"
   "emacs-browse-at-remote"
   "emacs-bubbleberry-theme"
   "emacs-bui"
   "emacs-busybee-theme"
   "emacs-centered-cursor-mode"
   "emacs-cfrs"
   "emacs-cherry-blossom-theme"
   "emacs-chocolate"
   "emacs-chocolate-theme"
   "emacs-cider"
   "emacs-cider-eval-sexp-fu"
   "emacs-cider-hydra"
   "emacs-clean-aindent-mode"
   "emacs-clj-refactor"
   "emacs-clojure-mode"
   "emacs-clojure-snippets"
   ;; "emacs-closql" ;; pulls-in the emacs-emacsql; using bste:emacs-closql
   "emacs-clues-theme"
   "emacs-cmm-mode"
   "emacs-color-identifiers-mode"
   "emacs-color-theme"        ;; obsolete - replaced by emacs-color-theme-modern
   "emacs-color-theme-modern"
   "emacs-color-theme-sanityinc-solarized"
   "emacs-color-theme-sanityinc-tomorrow"
   "emacs-color-theme-solarized"
   "emacs-column-enforce-mode"
   "emacs-company"
   "emacs-company-auctex"
   "emacs-company-cabal"
   "emacs-company-math"
   "emacs-company-php"
   "emacs-company-phpactor"
   "emacs-company-quickhelp"
   "emacs-company-reftex"
   "emacs-company-shell"
   "emacs-company-statistics"
   "emacs-company-web"
   "emacs-compat"
   "emacs-composer"
   ;; "emacs-copilot"
   "emacs-copy-sexp"
   "emacs-crux"
   "emacs-css-mode"
   "emacs-csv-mode"
   "emacs-cyberpunk-theme"
   "emacs-dakrone-theme"
   "emacs-dante"
   ;; "emacs-dap-mode"         ;; uses emacs-lsp-treemacs
   "emacs-darkmine-theme"
   "emacs-darkokai-theme"
   "emacs-darktooth-theme"
   "emacs-dash"
   "emacs-define-word"
   "emacs-desktop"
   "emacs-devdocs"
   "emacs-diff-hl"
   "emacs-diff-mode"
   "emacs-diminish"
   "emacs-dired-quick-sort"
   "emacs-dirvish"
   "emacs-django-theme"
   "emacs-doom-themes"
   "emacs-dotenv-mode"
   "emacs-dracula-theme"
   "emacs-drag-stuff"
   "emacs-drupal-mode"
   "emacs-dumb-jump"
   "emacs-edit-indirect"
   "emacs-editorconfig"
   "emacs-elisp-def"
   "emacs-elisp-refs"
   "emacs-elisp-slime-nav"
   "emacs-elm-mode"
   ;; "emacs-emacsql"         ;; using bste:emacs-emacsql
   ;; "emacs-emacsql-sqlite3" ;; pulls-in the emacs-emacsql; using bste:emacs-emacsql-sqlite3
   "emacs-emmet-mode"
   "emacs-emr"
   "emacs-epl"
   "emacs-erc"
   "emacs-erc-hl-nicks"
   "emacs-erc-image"
   "emacs-erc-social-graph"
   "emacs-erc-view-log"
   "emacs-erc-yt"
   "emacs-erlang"
   "emacs-eros"
   "emacs-esh-help"
   "emacs-eshell-prompt-extras"
   "emacs-eshell-z"
   "emacs-espresso-theme"
   "emacs-eval-in-repl"
   "emacs-eval-in-repl-cider"
   "emacs-eval-in-repl-elm"
   "emacs-eval-in-repl-erlang"
   "emacs-eval-in-repl-geiser"
   "emacs-eval-in-repl-hy"
   "emacs-eval-in-repl-ielm"
   "emacs-eval-in-repl-iex"
   "emacs-eval-in-repl-javascript"
   "emacs-eval-in-repl-lua"
   "emacs-eval-in-repl-ocaml"
   "emacs-eval-in-repl-prolog"
   "emacs-eval-in-repl-python"
   "emacs-eval-in-repl-racket"
   "emacs-eval-in-repl-ruby"
   "emacs-eval-in-repl-scheme"
   "emacs-eval-in-repl-shell"
   "emacs-eval-in-repl-slime"
   "emacs-eval-in-repl-sly"
   "emacs-eval-in-repl-sml"
   "emacs-eval-sexp-fu-el"
   "emacs-evil"
   "emacs-evil-anzu"
   "emacs-evil-args"
   "emacs-evil-cleverparens"
   "emacs-evil-collection"
   "emacs-evil-easymotion"
   "emacs-evil-escape"
   "emacs-evil-evilified-state"
   "emacs-evil-exchange"
   "emacs-evil-goggles"
   "emacs-evil-iedit-state"
   "emacs-evil-indent-plus"
   "emacs-evil-lion"
   "emacs-evil-lisp-state"
   "emacs-evil-matchit"
   "emacs-evil-mc"
   "emacs-evil-nerd-commenter"
   "emacs-evil-numbers"
   "emacs-evil-org"
   "emacs-evil-surround"
   "emacs-evil-tex"
   "emacs-evil-textobj-line"
   "emacs-evil-tutor"
   "emacs-evil-unimpaired"
   "emacs-evil-visual-mark-mode"
   "emacs-evil-visualstar"
   "emacs-exotica-theme"
   "emacs-expand-region"
   "emacs-eyebrowse"
   "emacs-eziam-themes"
   "emacs-f"
   "emacs-fancy-battery"
   "emacs-farmhouse-light-mod-theme"
   "emacs-farmhouse-themes"
   "emacs-fish-mode"
   "emacs-flatland-theme"
   "emacs-flatui-theme"
   "emacs-flx"
   "emacs-flx-ido"
   "emacs-flycheck"
   "emacs-flycheck-bashate"
   "emacs-flycheck-clj-kondo"
   "emacs-flycheck-elsa"
   "emacs-flycheck-haskell"
   "emacs-flycheck-package"
   "emacs-flycheck-pos-tip"
   "emacs-flyspell"
   "emacs-flyspell-correct"
   "emacs-flyspell-correct-helm"
   "emacs-font-lock+"
   "emacs-font-utils"
   ;; "emacs-forge" ;; pulls-in the emacs-emacsql; ; using bste:emacs-forge
   "emacs-fuzzy"
   "emacs-gandalf-theme"
   "emacs-geben"
   "emacs-geiser"
   "emacs-geiser-guile"
   "emacs-gh-md"
   "emacs-ghub"
   "emacs-git-commit"
   "emacs-git-link"
   "emacs-git-messenger"
   "emacs-git-modes"
   "emacs-git-timemachine"
   "emacs-gitignore-templates"
   "emacs-gntp"
   "emacs-gnuplot"
   "emacs-golden-ratio"
   "emacs-google-translate"
   "emacs-gotham-theme"
   "emacs-goto-chg"
   "emacs-grandshell-theme"
   "emacs-graphviz-dot-mode"
   "emacs-groovy-imports"
   "emacs-groovy-mode"
   "emacs-gruber-darker"
   "emacs-gruber-darker-theme"
   "emacs-gruvbox-theme"
   "emacs-haml-mode"
   "emacs-haskell-mode"
   "emacs-haskell-snippets"
   "emacs-hc-zenburn-theme"
   "emacs-helm"
   "emacs-helm-ag"
   "emacs-helm-c-yasnippet"
   "emacs-helm-cider"
   "emacs-helm-cider-history"
   "emacs-helm-company"
   "emacs-helm-css-scss"
   "emacs-helm-descbinds"
   "emacs-helm-dictionary"
   "emacs-helm-git-grep"
   "emacs-helm-hoogle"
   "emacs-helm-ls-git"
   "emacs-helm-lsp"
   "emacs-helm-make"
   "emacs-helm-mode-manager"
   "emacs-helm-org"
   "emacs-helm-org-rifle"
   "emacs-helm-projectile"
   "emacs-helm-purpose"
   "emacs-helm-slime"
   "emacs-helm-sly"
   "emacs-helm-swoop"
   "emacs-helm-system-packages"
   "emacs-helm-themes"
   "emacs-helm-xref"
   "emacs-help-fns+"
   "emacs-hemisu-theme"
   "emacs-heroku-theme"
   "emacs-hide-comnt"
   "emacs-highlight-indentation"
   "emacs-highlight-numbers"
   "emacs-highlight-parentheses"
   "emacs-hindent"
   "emacs-hippie-exp"
   "emacs-hl-todo"
   "emacs-hlint-refactor"
   "emacs-holy-mode"
   "emacs-ht"
   "emacs-htmlize"
   "emacs-hungry-delete"
   "emacs-hy-mode"
   "emacs-hybrid-mode"
   "emacs-hydra"
   "emacs-iedit"
   "emacs-imenu-list"
   "emacs-impatient-mode"
   "emacs-indent-guide"
   "emacs-inf-ruby"
   "emacs-inflections"
   "emacs-info+"
   "emacs-inkpot-theme"
   "emacs-insert-shebang"
   "emacs-inspector"
   "emacs-ir-black-theme"
   "emacs-jazz-theme"
   "emacs-jbeans-theme"
   "emacs-js-comint"
   "emacs-js-doc"
   "emacs-js2-mode"
   "emacs-js2-refactor"
   "emacs-js2-refactor-el"
   "emacs-json-mode"
   "emacs-json-navigator"
   "emacs-json-reformat"
   "emacs-json-snatcher"
   "emacs-jump-last"
   "emacs-kaolin-themes"
   "emacs-kbd-mode"
   "emacs-key-chord"
   "emacs-kill-buffers"
   "emacs-lcr"
   "emacs-less-css-mode"
   "emacs-ligature"
   "emacs-light-soap-theme"
   "emacs-link-hint"
   "emacs-livid-mode"
   "emacs-log4e"
   "emacs-loop"
   "emacs-lorem-ipsum"
   "emacs-lsp-haskell"
   ;; "emacs-lsp-java"     ;; uses emacs-dap-mode
   "emacs-lsp-latex"
   ;; "emacs-lsp-metals"    ;; uses emacs-dap-mode
   "emacs-lsp-mode"
   "emacs-lsp-origami"
   "emacs-lsp-python-ms"
   ;; "emacs-lsp-treemacs"    ;; uses emacs-treemacs
   "emacs-lsp-ui"
   "emacs-lsp-volar"
   "emacs-lua-mode"
   "emacs-lush-theme"
   "emacs-macrostep"
   "emacs-madhat2r-theme"
   "emacs-magit"
   "emacs-magit-annex"
   "emacs-magit-gerrit"
   "emacs-magit-org-todos-el"
   "emacs-magit-popup"
   "emacs-magit-section"
   "emacs-magit-todos"
   "emacs-majapahit-themes"
   "emacs-map"
   "emacs-markdown-mode"
   "emacs-markdown-toc"
   "emacs-material-theme"
   "emacs-math-symbol-lists"
   "emacs-maven-test-mode"
   "emacs-memoize"
   "emacs-minimal-theme"
   "emacs-mmm-mode"
   "emacs-modus-themes"
   "emacs-moe-theme"
   "emacs-monochrome-theme"
   "emacs-monokai-theme"
   "emacs-multi-line"
   "emacs-multi-term"
   "emacs-multi-vterm"
   "emacs-multiple-cursors"
   "emacs-mustang-theme"
   "emacs-mvn"
   "emacs-mwim"
   "emacs-nameless"
   "emacs-naquadah-theme"
   "emacs-noctilux-theme"
   "emacs-nodejs-repl"
   "emacs-npm-mode"
   "emacs-ob-erlang"
   "emacs-ob-racket"
   "emacs-obsidian-theme"
   "emacs-occidental"
   "emacs-occidental-theme"
   "emacs-oldlace-theme"
   "emacs-omtose-phellack-theme"
   "emacs-omtose-phellack-theme" ;; defines emacs-omtose-darker-theme emacs-omtose-softer-theme
   "emacs-open-junk-file"
   "emacs-org"
   "emacs-org-cliplink"
   "emacs-org-contrib"
   "emacs-org-download"
   "emacs-org-mime"
   "emacs-org-pomodoro"
   "emacs-org-present"
   "emacs-org-projectile"
   "emacs-org-rich-yank"
   "emacs-org-superstar"
   "emacs-organic-green-theme"
   "emacs-orgit"
   "emacs-orgit-forge"
   "emacs-origami"
   "emacs-origami-el" ;; this one is probably not needed
   "emacs-overseer"
   "emacs-package-lint"
   "emacs-packed"
   "emacs-paradox"
   "emacs-paredit"
   "emacs-parent-mode"
   "emacs-parseclj"
   "emacs-parseedn"
   "emacs-password-generator"
   "emacs-pcre2el"
   "emacs-pdf-tools"
   "emacs-pdf-view-restore"
   "emacs-persp-mode"
   "emacs-perspective"
   "emacs-pfuture"
   "emacs-phoenix-dark-mono-theme"
   "emacs-phoenix-dark-pink-theme"
   "emacs-php-auto-yasnippets"
   "emacs-php-extras"
   "emacs-php-mode"
   "emacs-phpactor"
   "emacs-phpcbf"
   "emacs-phpunit"
   "emacs-pippel"
   "emacs-pkg-info"
   "emacs-planet"
   "emacs-planet-theme"
   "emacs-pollen-mode"
   "emacs-popup"
   "emacs-popwin"
   "emacs-pos-tip"
   "emacs-posframe"
   "emacs-powerline"
   "emacs-prettier-js"
   "emacs-professional-theme"
   "emacs-projectile"
   "emacs-pug-mode"
   "emacs-purple-haze-theme"
   "emacs-queue"
   "emacs-quickrun"
   "emacs-racket-mode"
   "emacs-railscasts-theme"
   "emacs-rainbow-delimiters"
   "emacs-rainbow-identifiers"
   "emacs-rainbow-mode"
   "emacs-rebecca-theme"
   "emacs-reformatter"
   "emacs-request"
   "emacs-restart-emacs"
   "emacs-reverse-theme"
   "emacs-s"
   "emacs-sass-mode"
   "emacs-sayid"
   "emacs-sbt-mode"
   "emacs-scala-mode"
   "emacs-scribble-mode"
   "emacs-scss-mode"
   "emacs-seq"
   "emacs-sesman"
   "emacs-seti-theme"
   "emacs-shell-pop"
   "emacs-shfmt"
   "emacs-shut-up"
   "emacs-simple-httpd"
   "emacs-skewer-mode"
   "emacs-slim-mode"
   "emacs-slime"
   "emacs-slime-company"
   "emacs-slime-repl-ansi-color"
   "emacs-slime-volleyball"
   "emacs-sly"
   "emacs-sly-asdf"
   "emacs-sly-macrostep"
   "emacs-sly-named-readtables"
   "emacs-sly-package-inferred"
   "emacs-sly-quicklisp"
   "emacs-sly-stepper"
   "emacs-smartparens"
   "emacs-smeargle"
   "emacs-sml-mode"
   "emacs-smyx-theme"
   "emacs-soft-charcoal-theme"
   "emacs-soft-morning-theme"
   "emacs-soft-stone-theme"
   "emacs-solarized-theme"
   "emacs-soothe-theme"
   "emacs-space-doc"
   "emacs-spacegray-theme"
   "emacs-spaceline"
   "emacs-spaceline-all-the-icons"
   "emacs-spacemacs-purpose-popwin"
   "emacs-spacemacs-whitespace-cleanup"
   "emacs-sphinx-doc"
   "emacs-spinner"
   "emacs-sql"
   "emacs-sql-indent"
   "emacs-sqlite"
   "emacs-sqlite3-api"
   "emacs-string-edit-at-point"
   "emacs-string-inflection"
   "emacs-subatomic-theme"
   "emacs-subatomic256-theme"
   "emacs-sublime-themes"
   "emacs-suggest"
   "emacs-sunny-day-theme"
   "emacs-super-save"
   "emacs-symbol-overlay"
   "emacs-symon"
   "emacs-tablist"
   "emacs-tagedit"
   "emacs-tango-2-theme"
   "emacs-tango-plus-theme"
   "emacs-tangotango-theme"
   "emacs-tao-theme"
   ;; "emacs-taxy-magit-section" ;; this seems not to be the magit-section
   "emacs-term-cursor"
   "emacs-terminal-here"
   "emacs-tide"
   "emacs-tldr" ;; access `tldr' pages from within Emacs
   "emacs-toc-org"
   "emacs-toxi-theme"
   "emacs-tramp"
   "emacs-tramp-auto-auth"
   "emacs-transient"
   "emacs-transpose-frame"
   ;; "emacs-treemacs"        ;; using the package from the 'bost' channel
   ;; "emacs-treemacs-evil"
   ;; "emacs-treemacs-icons-dired"
   ;; "emacs-treemacs-magit"  ;; existing package
   ;; "emacs-treemacs-mu4e"   ;; existing package
   ;; "emacs-treemacs-persp"
   ;; "emacs-treemacs-projectile"
   "emacs-treepy"
   "emacs-tweaks"
   "emacs-twilight-anti-bright-theme"
   "emacs-twilight-bright-theme"
   "emacs-twilight-theme"
   "emacs-typescript-mode"
   "emacs-ujelly-theme"
   "emacs-underwater-theme"
   "emacs-undo-tree"
   "emacs-unfill"
   "emacs-unicode-fonts"
   "emacs-use-package"
   "emacs-use-package-chords"
   "emacs-uuidgen"
   "emacs-vi-tilde-fringe"
   "emacs-vim-powerline"
   "emacs-visual-fill-column"
   "emacs-volatile-highlights"
   "emacs-vterm"
   "emacs-web-beautify"
   "emacs-web-completion-data"
   "emacs-web-mode"
   "emacs-which-key"
   "emacs-white-sand-theme"
   "emacs-window-purpose"
   "emacs-winum"
   "emacs-writeroom-mode"
   "emacs-ws-butler"
   "emacs-xcscope"
   "emacs-xref"
   "emacs-xterm-color"
   "emacs-yaml"
   "emacs-yaml-mode"
   "emacs-yasnippet"
   "emacs-yasnippet-snippets"
   "emacs-zen-and-art-theme"
   "emacs-zenburn-theme"
   "emacs-zonokai-emacs"
   "emacs-zop-to-char"
   ;; "emacs-auto-yasnippet"
   ;; "emacs-guix"
   ;; "emacs-paradox"
   ;; Overridden by pkg:... in the $dotf/guix/home/cfg/packages/all.scm
   ))
(testsymb 'needed-packages)

(module-evaluated)
