(define-module (cfg packages spguimacs needed)
  #:use-module (srfi srfi-1)
  #:use-module (utils) ;; partial, m
  #:export (
            needed-packages
            ))

(define m (module-name-for-logging))
;; (format #t "~a evaluating module ...\n" m)

;; Specifying package using @, ie. "(@ (gnu packages emacs-xyz) emacs-guix)"
;; doesn't work
(define needed-packages
  (list
   "emacs-copilot"
   "emacs-centered-cursor-mode"
   "emacs-company-statistics"
   "emacs-json-navigator"
   "emacs-eziam-themes"
   "emacs-tangotango"
   "emacs-helm-cider-history"
   "emacs-flx"
   "emacs-twilight-bright"
   "emacs-haskell-snippets"
   "emacs-lsp-haskell"
   "emacs-darkmine"
   "emacs-helm-css-scss"
   ;; "emacs-auto-yasnippet"
   "emacs-composer"
   "emacs-soft-stone"
   "emacs-twilight-anti-bright"
   "emacs-erc-social-graph"
   "emacs-blacken"
   "emacs-hlint-refactor"
   "emacs-chocolate"
   "emacs-soft-charcoal"
   "emacs-clues"
   "emacs-planet"
   "emacs-occidental"
   "emacs-gruber-darker"
   "emacs-vi-tilde-fringe"
   "emacs-popwin"
   ;; "emacs-paradox"
   "emacs-lsp-volar"
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
   "emacs-annalist"
   "emacs-anti-zenburn-theme"
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
   "emacs-browse-at-remote"
   "emacs-bubbleberry-theme"
   "emacs-bui"
   "emacs-busybee-theme"
   "emacs-cfrs"
   "emacs-cherry-blossom-theme"
   "emacs-chocolate-theme"
   "emacs-cider"
   "emacs-cider-eval-sexp-fu"
   "emacs-cider-hydra"
   "emacs-clean-aindent-mode"
   "emacs-clj-refactor"
   "emacs-clojure-mode"
   "emacs-clojure-snippets"
   "emacs-clues-theme"
   "emacs-cmm-mode"
   "emacs-color-identifiers-mode"
   "emacs-color-theme-sanityinc-solarized"
   "emacs-color-theme-sanityinc-tomorrow"
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
   "emacs-company-web"
   "emacs-compat"
   "emacs-copy-sexp"
   "emacs-crux"
   "emacs-css-mode"
   "emacs-csv-mode"
   "emacs-cyberpunk-theme"
   "emacs-dakrone-theme"
   "emacs-dante"
   "emacs-dap-mode"
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
   "emacs-editorconfig"
   "emacs-elisp-def"
   "emacs-elisp-slime-nav"
   "emacs-emmet-mode"
   "emacs-emr"
   "emacs-epl"
   "emacs-erc"
   "emacs-erc-hl-nicks"
   "emacs-erc-image"
   "emacs-erc-view-log"
   "emacs-erc-yt"
   "emacs-esh-help"
   "emacs-eshell-prompt-extras"
   "emacs-eshell-z"
   "emacs-espresso-theme"
   "emacs-eval-sexp-fu"
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
   "emacs-f"
   "emacs-fancy-battery"
   "emacs-farmhouse-light-mod-theme"
   "emacs-farmhouse-themes"
   "emacs-fish-mode"
   "emacs-flatland-theme"
   "emacs-flatui-theme"
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
   "emacs-forge"
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
   "emacs-grandshell-theme"
   "emacs-graphviz-dot-mode"
   "emacs-groovy-imports"
   "emacs-groovy-mode"
   "emacs-gruber-darker-theme"
   "emacs-gruvbox-theme"

   ;; Overridden by pkg:... in the $dotf/guix/home/cfg/packages/all.scm
   ;; "emacs-guix"

   "emacs-haml-mode"
   "emacs-haskell-mode"
   "emacs-hc-zenburn-theme"
   "emacs-helm"
   "emacs-helm-ag"
   "emacs-helm-c-yasnippet"
   "emacs-helm-cider"
   "emacs-helm-company"
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
   "emacs-holy-mode"
   "emacs-ht"
   "emacs-htmlize"
   "emacs-hungry-delete"
   "emacs-hybrid-mode"
   "emacs-hydra"
   "emacs-imenu-list"
   "emacs-impatient-mode"
   "emacs-indent-guide"
   "emacs-inflections"
   "emacs-info+"
   "emacs-inkpot-theme"
   "emacs-insert-shebang"
   "emacs-inspector"
   "emacs-ir-black-theme"
   "emacs-jazz-theme"
   "emacs-jbeans-theme"
   "emacs-js-doc"
   "emacs-js2-mode"
   "emacs-js2-refactor"
   "emacs-json-mode"
   "emacs-json-reformat"
   "emacs-json-snatcher"
   "emacs-jump-last"
   "emacs-kaolin-themes"
   "emacs-kbd-mode"
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
   "emacs-lsp-java"
   "emacs-lsp-latex"
   "emacs-lsp-metals"
   "emacs-lsp-mode"
   "emacs-lsp-origami"
   "emacs-lsp-python-ms"
   "emacs-lsp-treemacs"
   "emacs-lsp-ui"
   "emacs-lush"
   "emacs-lush-theme"
   "emacs-macrostep"
   "emacs-madhat2r-theme"
   "emacs-magit"
   "emacs-magit-section"
   "emacs-magit-todos"
   "emacs-majapahit-theme"
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
   "emacs-molokai-theme"
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
   "emacs-ob-racket"
   "emacs-obsidian-theme"
   "emacs-occidental-theme"
   "emacs-oldlace-theme"
   "emacs-omtose-phellack-theme"
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
   "emacs-overseer"
   "emacs-package-lint"
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
   "emacs-planet-theme"
   "emacs-pollen-mode"
   "emacs-popup"
   "emacs-posframe"
   "emacs-powerline"
   "emacs-prettier-js"
   "emacs-professional-theme"
   "emacs-projectile"
   "emacs-pug-mode"
   "emacs-purple-haze-theme"
   "emacs-quickrun"
   "emacs-racket-mode"
   "emacs-railscasts-theme"
   "emacs-rainbow-delimiters"
   "emacs-rainbow-identifiers"
   "emacs-rainbow-mode"
   "emacs-rebecca-theme"
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
   "emacs-sesman"
   "emacs-seti-theme"
   "emacs-shell-pop"
   "emacs-shfmt"
   "emacs-shut-up"
   "emacs-simple-httpd"
   "emacs-skewer-mode"
   "emacs-slim-mode"
   "emacs-smartparens"
   "emacs-smeargle"
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
   "emacs-sql"
   "emacs-sql-indent"
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
   "emacs-term-cursor"
   "emacs-terminal-here"
   "emacs-tide"
   "emacs-toc-org"
   "emacs-toxi-theme"
   "emacs-transient"
   "emacs-transpose-frame"
   "emacs-treemacs"
   "emacs-treemacs-evil"
   "emacs-treemacs-icons-dired"
   "emacs-treemacs-persp"
   "emacs-treemacs-projectile"
   "emacs-treepy"
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
   "emacs-vim-powerline"
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
   "emacs-xterm-color"
   "emacs-yaml"
   "emacs-yaml-mode"
   "emacs-yasnippet"
   "emacs-yasnippet-snippets"
   "emacs-zen-and-art-theme"
   "emacs-zenburn-theme"
   "emacs-zonokai-emacs"
   "emacs-zop-to-char"
   ))
(testsymb 'needed-packages)
