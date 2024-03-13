(define-module (cfg packages spguimacs available)
  #:use-module (srfi srfi-1)
  #:use-module (utils) ;; partial, m
  #:use-module (gnu packages) ;; find-packages-by-name
  #:use-module (guix)         ;; package-name
)

(evaluating-module)

;; guix package --list-available='^emacs-' | awk '{print "\""$1"\""}'
;; Specifying package using @, ie. "(@ (gnu packages emacs-xyz) emacs-guix)"
;; doesn't work
(define (guix-package---list-available)
  (list
   "emacs-2048-game"
   "emacs-4clojure"
   "emacs-9lc-mode"
   "emacs-a"
   "emacs-abyss-theme"
   "emacs-ac-geiser"
   "emacs-ac-ispell"
   "emacs-ac-php"
   "emacs-academic-phrases"
   "emacs-ace-jump-helm-line"
   "emacs-ace-jump-mode"
   "emacs-ace-link"
   "emacs-ace-window"
   "emacs-acme-theme"
   "emacs-activities"
   "emacs-adaptive-wrap"
   "emacs-add-hooks"
   "emacs-add-node-modules-path"
   "emacs-adoc-mode"
   "emacs-adwaita-dark-theme"
   "emacs-afternoon-theme"
   "emacs-ag"
   "emacs-agda2-mode"
   "emacs-aggressive-indent"
   "emacs-ahg"
   "emacs-ahungry-theme"
   "emacs-aio"
   "emacs-airline-themes"
   "emacs-alarm-clock"
   "emacs-alchemist"
   "emacs-alect-themes"
   "emacs-alert"
   "emacs-all-the-icons"
   "emacs-all-the-icons-completion"
   "emacs-all-the-icons-dired"
   "emacs-all-the-icons-ibuffer"
   "emacs-almost-mono-themes"
   "emacs-alsamixer-el"
   "emacs-ample-regexps"
   "emacs-ample-theme"
   "emacs-ample-zen-theme"
   "emacs-amx"
   ;; "emacs-anaconda-mode" ;; requires emacs-pythonic which fails to compile
   "emacs-anakondo"
   "emacs-anaphora"
   "emacs-anki-editor"
   "emacs-annalist"
   "emacs-ansi"
   "emacs-ansible-doc"
   "emacs-anti-zenburn-theme"
   "emacs-anzu"
   "emacs-apache-mode"
   "emacs-apel-lb"
   "emacs-apheleia"
   "emacs-app-launcher"
   "emacs-apropospriate-theme"
   "emacs-arduino-mode"
   "emacs-arei"
   "emacs-ascii-art-to-unicode"
   "emacs-async"
   "emacs-async-await"
   "emacs-atom-one-dark-theme"
   "emacs-attrap"
   "emacs-auctex"
   "emacs-auth-source-pass"
   "emacs-auth-source-xoauth2"
   "emacs-auto-compile"
   "emacs-auto-complete"
   "emacs-auto-dictionary-mode"
   "emacs-auto-sudoedit"
   "emacs-auto-yasnippet"
   "emacs-autocrypt"
   "emacs-autothemer"
   "emacs-avy"
   "emacs-back-button"
   "emacs-badwolf-theme"
   "emacs-base16-theme"
   "emacs-bash-completion"
   "emacs-bazel"
   "emacs-bbdb"
   "emacs-bbdb-vcard"
   "emacs-beacon"
   "emacs-beancount"
   "emacs-beframe"
   "emacs-beginend"
   "emacs-benchmark-init"
   "emacs-better-defaults"
   "emacs-bfuture"
   "emacs-biblio"
   "emacs-bind-map"
   "emacs-bing-dict"
   "emacs-birds-of-paradise-plus-theme"
   "emacs-bison-mode"
   "emacs-bitbake-modes"
   "emacs-blacken"
   "emacs-blackout"
   "emacs-blight"
   "emacs-blimp"
   "emacs-bluetooth"
   "emacs-bm"
   "emacs-bongo"
   "emacs-bookmark-plus"
   "emacs-boon"
   "emacs-boxquote"
   "emacs-browse-at-remote"
   "emacs-browse-kill-ring"
   "emacs-bubbleberry-theme"
   "emacs-buffer-env"
   "emacs-buffer-move"
   "emacs-bug-hunter"
   "emacs-bui"
   "emacs-build-farm"
   "emacs-burly"
   "emacs-busybee-theme"
   "emacs-butler"
   "emacs-buttercup"
   "emacs-cal-china-x"
   "emacs-calc-currency"
   "emacs-calfw"
   "emacs-calibredb"
   "emacs-caml"
   "emacs-cape"
   "emacs-caps-lock"
   "emacs-carp"
   "emacs-cascading-dir-locals"
   "emacs-cc-mode"
   "emacs-ccls"
   "emacs-cdlatex"
   "emacs-centered-cursor-mode"
   "emacs-cfrs"
   "emacs-chatgpt-shell"
   "emacs-cherry-blossom-theme"
   "emacs-chess"
   "emacs-chocolate"
   "emacs-chocolate-theme"
   "emacs-chronometrist"
   "emacs-cider"
   "emacs-cider-eval-sexp-fu"
   "emacs-cider-hydra"
   "emacs-circadian"
   "emacs-circe"
   "emacs-citar"
   "emacs-citar-denote"
   "emacs-citar-org-roam"
   "emacs-citeproc-el"
   "emacs-citre"
   "emacs-cl-print"
   "emacs-clang-format"
   "emacs-clang-rename"
   "emacs-clj-deps-new"
   "emacs-clj-refactor"
   "emacs-clojure-mode"
   "emacs-clojure-snippets"
   "emacs-closql"
   "emacs-clue"
   "emacs-clues-theme"
   "emacs-cmake-font-lock"
   "emacs-cmake-mode"
   "emacs-cnfonts"
   "emacs-code-cells"
   "emacs-color-identifiers-mode"
   "emacs-color-theme"        ;; obsolete - replaced by emacs-color-theme-modern
   "emacs-color-theme-modern"
   "emacs-color-theme-sanityinc-solarized"
   "emacs-color-theme-sanityinc-tomorrow"
   "emacs-color-theme-solarized"
   "emacs-column-enforce-mode"
   "emacs-column-marker"
   "emacs-combobulate"
   "emacs-commander"
   "emacs-company"
   "emacs-company-auctex"
   "emacs-company-box"
   "emacs-company-cabal"
   "emacs-company-coq"
   "emacs-company-ebdb"
   "emacs-company-emoji"
   "emacs-company-flow"
   "emacs-company-irony"
   "emacs-company-jedi"
   "emacs-company-lsp"
   "emacs-company-lua"
   "emacs-company-math"
   "emacs-company-org-block"
   "emacs-company-posframe"
   "emacs-company-quickhelp"
   "emacs-company-reftex"
   "emacs-company-restclient"
   "emacs-company-statistics"
   "emacs-company-web"
   "emacs-compat"
   "emacs-compdef"
   "emacs-composer"
   "emacs-constants"
   "emacs-consult"
   "emacs-consult-bibtex"
   "emacs-consult-dir"
   "emacs-consult-eglot"
   "emacs-consult-flycheck"
   "emacs-consult-lsp"
   "emacs-consult-notmuch"
   "emacs-consult-org-roam"
   "emacs-consult-xdg-recent-files"
   "emacs-consult-yasnippet"
   "emacs-copilot"
   "emacs-copy-sexp"
   "emacs-corfu"
   "emacs-corfu-candidate-overlay"
   "emacs-corfu-doc"
   "emacs-corfu-doc-terminal"
   "emacs-corfu-terminal"
   "emacs-cort"
   "emacs-coterm"
   "emacs-counsel"
   "emacs-counsel-bbdb"
   "emacs-counsel-dash"
   "emacs-counsel-etags"
   "emacs-counsel-jq"
   "emacs-counsel-notmuch"
   "emacs-counsel-projectile"
   "emacs-counsel-tramp"
   "emacs-cov"
   "emacs-cpreproc"
   "emacs-cpreproc-openvdb"
   "emacs-crdt"
   "emacs-crux"
   "emacs-csharp-mode"
   "emacs-csound-mode"
   "emacs-csv"
   "emacs-csv-mode"
   "emacs-ctable"
   "emacs-ctrlf"
   "emacs-cwl-mode"
   "emacs-cyberpunk-theme"
   "emacs-cyrillic-dvorak-im"
   "emacs-d-mode"
   "emacs-daemons"
   "emacs-dakrone-theme"
   "emacs-danneskjold-theme"
   "emacs-dante"
   "emacs-dap-mode"
   "emacs-darkmine-theme"
   "emacs-darkokai-theme"
   "emacs-darkroom"
   "emacs-darktooth-theme"
   "emacs-dart-mode"
   "emacs-dash"
   "emacs-dash-docs"
   "emacs-dashboard"
   "emacs-datetime"
   "emacs-ddskk"
   "emacs-ddskk-nicola"
   "emacs-deadgrep"
   "emacs-debase"
   "emacs-debbugs"
   "emacs-debpaste"
   "emacs-dedicated"
   "emacs-default-encrypt"
   "emacs-default-text-scale"
   "emacs-deferred"
   "emacs-deft"
   "emacs-delight"
   "emacs-denote"
   "emacs-denote-menu"
   "emacs-desktop-environment"
   "emacs-detached"
   "emacs-devdocs"
   "emacs-dhall-mode"
   "emacs-dictionary"
   "emacs-diff-hl"
   "emacs-diminish"
   "emacs-dimmer"
   "emacs-dired-du"
   "emacs-dired-git-info"
   "emacs-dired-hacks"
   "emacs-dired-preview"
   "emacs-dired-rsync"
   "emacs-dired-sidebar"
   "emacs-dired-toggle-sudo"
   "emacs-diredfl"
   "emacs-direnv"
   "emacs-dirvish"
   "emacs-disable-mouse"
   "emacs-discomfort"
   "emacs-discover"
   "emacs-discover-my-major"
   "emacs-disk-usage"
   "emacs-display-wttr"
   "emacs-diss"
   "emacs-django-theme"
   "emacs-djvu"
   "emacs-djvu3"
   "emacs-dmenu"
   "emacs-dnt"
   "emacs-docker"
   "emacs-docker-compose-mode"
   "emacs-docker-tramp"
   "emacs-dockerfile-mode"
   "emacs-doom-modeline"
   "emacs-doom-snippets"
   "emacs-doom-themes"
   "emacs-dotenv-mode"
   "emacs-download-region"
   "emacs-dpd"
   "emacs-dracula-theme"
   "emacs-drag-stuff"
   "emacs-dream-theme"
   "emacs-dtrt-indent"
   "emacs-dts-mode"
   "emacs-dumb-jump"
   "emacs-dumbparens"
   "emacs-dvc"
   "emacs-e2wm"
   "emacs-eacl"
   "emacs-easy-kill"
   "emacs-eat"
   "emacs-ebdb"
   "emacs-ebdb-i18n-chn"
   "emacs-ebib"
   "emacs-ebuild-mode"
   "emacs-ebuku"
   "emacs-ecukes"
   "emacs-ed-mode"
   "emacs-edbi"
   "emacs-edbi-sqlite"
   "emacs-ediprolog"
   "emacs-edit-indirect"
   "emacs-edit-server"
   "emacs-editorconfig"
   "emacs-edn"
   "emacs-ednc"
   "emacs-ef-themes"
   "emacs-eglot"
   "emacs-eglot-tempel"
   "emacs-eglot-x"
   "emacs-eimp"
   "emacs-ein"
   "emacs-el-mock"
   "emacs-el-patch"
   "emacs-el-search"
   "emacs-el-x"
   "emacs-el2org"
   "emacs-eldev"
   "emacs-eldoc"
   "emacs-eldoc-box"
   "emacs-elegant-agenda-mode"
   "emacs-elf-mode"
   "emacs-elfeed"
   "emacs-elfeed-goodies"
   "emacs-elfeed-org"
   "emacs-elfeed-protocol"
   "emacs-elfeed-score"
   "emacs-eli"
   "emacs-elisp-demos"
   "emacs-elisp-docstring-mode"
   "emacs-elisp-refs"
   "emacs-elisp-slime-nav"
   "emacs-elixir-mode"
   "emacs-elm-mode"
   "emacs-elmacro"
   "emacs-elpher"
   "emacs-elpy"
   "emacs-elquery"
   "emacs-emacsql"
   "emacs-emacsql-sqlite3"
   "emacs-emamux"
   "emacs-embark"
   "emacs-ement"
   "emacs-emmet-mode"
   "emacs-emms"
   "emacs-emms-mode-line-cycle"
   "emacs-emojify"
   "emacs-emprise"
   "emacs-engine-mode"
   "emacs-engrave-faces"
   "emacs-enh-ruby-mode"
   "emacs-enlive"
   "emacs-envrc"
   "emacs-epc"
   "emacs-epithet"
   "emacs-epkg"
   "emacs-epl"
   "emacs-eprime"
   "emacs-equake"
   "emacs-eradio"
   "emacs-erc-hl-nicks"
   "emacs-erc-image"
   "emacs-erc-scrolltoplace"
   "emacs-erc-social-graph"
   "emacs-erc-status-sidebar"
   "emacs-ergoemacs-mode"
   "emacs-erlang"
   "emacs-eros"
   "emacs-ert-async"
   "emacs-ert-expectations"
   "emacs-ert-runner"
   "emacs-es-mode"
   "emacs-esh-autosuggest"
   "emacs-esh-help"
   "emacs-eshell-bookmark"
   "emacs-eshell-did-you-mean"
   "emacs-eshell-prompt-extras"
   "emacs-eshell-syntax-highlighting"
   "emacs-eshell-toggle"
   "emacs-eshell-up"
   "emacs-eshell-z"
   "emacs-espresso-theme"
   "emacs-espuds"
   "emacs-ess"
   "emacs-esup"
   "emacs-esxml"
   "emacs-eterm-256color"
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
   "emacs-everywhere"
   "emacs-evil"
   "emacs-evil-anzu"
   "emacs-evil-args"
   "emacs-evil-cleverparens"
   "emacs-evil-collection"
   "emacs-evil-commentary"
   "emacs-evil-escape"
   "emacs-evil-exchange"
   "emacs-evil-expat"
   "emacs-evil-goggles"
   "emacs-evil-iedit-state"
   "emacs-evil-indent-plus"
   "emacs-evil-leader"
   "emacs-evil-lion"
   "emacs-evil-markdown"
   "emacs-evil-matchit"
   "emacs-evil-mc"
   "emacs-evil-multiedit"
   "emacs-evil-nerd-commenter"
   "emacs-evil-numbers"
   "emacs-evil-org"
   "emacs-evil-owl"
   "emacs-evil-paredit"
   "emacs-evil-quickscope"
   "emacs-evil-replace-with-register"
   "emacs-evil-smartparens"
   "emacs-evil-surround"
   "emacs-evil-tex"
   "emacs-evil-text-object-python"
   "emacs-evil-textobj-syntax"
   "emacs-evil-tmux-navigator"
   "emacs-evil-traces"
   "emacs-evil-visual-replace"
   "emacs-evil-visualstar"
   "emacs-eweouz"
   "emacs-ewmctrl"
   "emacs-eww-lnum"
   "emacs-excorporate"
   "emacs-exec-path-from-shell"
   "emacs-execline"
   "emacs-exiftool"
   "emacs-exotica-theme"
   "emacs-expand-region"
   "emacs-explain-pause-mode"
   "emacs-extempore-mode"
   "emacs-extend-smime"
   "emacs-external-completion"
   "emacs-extmap"
   "emacs-exwm"
   "emacs-exwm-edit"
   "emacs-exwm-firefox"
   "emacs-exwm-firefox-core"
   "emacs-exwm-mff"
   "emacs-exwm-modeline"
   "emacs-exwm-no-x-toolkit"
   "emacs-exwm-ss"
   "emacs-exwm-x"
   "emacs-eyebrowse"
   "emacs-eziam-themes"
   "emacs-f"
   "emacs-f3"
   "emacs-faceup"
   "emacs-fancy-battery"
   "emacs-fancy-narrow"
   "emacs-farmhouse-light-mod-theme"
   "emacs-farmhouse-themes"
   "emacs-fb2-reader"
   "emacs-fd"
   "emacs-fdroid"
   "emacs-feature-mode"
   "emacs-fennel-mode"
   "emacs-ffap-rfc-space"
   "emacs-fill-column-indicator"
   "emacs-filladapt"
   "emacs-finalize"
   "emacs-find-file-in-project"
   "emacs-fish-completion"
   "emacs-fish-mode"
   "emacs-fixed-pitch"
   "emacs-flatland-theme"
   "emacs-flatui-theme"
   "emacs-flexoki-themes"
   "emacs-flim-lb"
   "emacs-flow-minor-mode"
   "emacs-flx"
   "emacs-flycheck"
   "emacs-flycheck-clj-kondo"
   "emacs-flycheck-cpplint"
   "emacs-flycheck-elm"
   "emacs-flycheck-flow"
   "emacs-flycheck-grammalecte"
   "emacs-flycheck-guile"
   "emacs-flycheck-haskell"
   "emacs-flycheck-irony"
   "emacs-flycheck-joker"
   "emacs-flycheck-ledger"
   "emacs-flycheck-package"
   "emacs-flycheck-rust"
   "emacs-flymake-collection"
   "emacs-flymake-flycheck"
   "emacs-flymake-guile"
   "emacs-flymake-kondor"
   "emacs-flymake-mypy"
   "emacs-flymake-popon"
   "emacs-flymake-proselint"
   "emacs-flymake-quickdef"
   "emacs-flymake-shellcheck"
   "emacs-flyspell-correct"
   "emacs-focus"
   "emacs-fold-dwim"
   "emacs-font-lock+"
   "emacs-font-lock-studio"
   "emacs-font-utils"
   "emacs-fontaine"
   "emacs-forge"
   "emacs-form-feed"
   "emacs-fountain-mode"
   "emacs-frame-local"
   "emacs-frame-purpose"
   "emacs-framemove"
   "emacs-frames-only-mode"
   "emacs-frecency"
   "emacs-free-keys"
   "emacs-fringe-helper"
   "emacs-frog-jump-buffer"
   "emacs-frog-menu"
   "emacs-frowny"
   "emacs-fsm"
   "emacs-fullframe"
   "emacs-function-args"
   "emacs-fzf"
   "emacs-gandalf-theme"
   "emacs-gc-stats"
   "emacs-gcmh"
   "emacs-gdscript-mode"

   ;; Overridden by pkg:... in the $dotf/guix/home/cfg/packages/all.scm
   ;; "emacs-geiser"

   "emacs-geiser-chez"
   "emacs-geiser-eros"
   "emacs-geiser-gauche"

   ;; Overridden by pkg:... in the $dotf/guix/home/cfg/packages/all.scm
   ;; "emacs-geiser-guile"

   "emacs-geiser-racket"
   "emacs-gemini"
   "emacs-general"
   "emacs-geoclue"
   "emacs-gerbil-mode"
   "emacs-ggtags"
   "emacs-ghq"
   "emacs-ghub"
   "emacs-gif-screencast"
   "emacs-git-annex"
   "emacs-git-auto-commit-mode"
   "emacs-git-email"
   "emacs-git-gutter"
   "emacs-git-gutter-fringe"
   "emacs-git-link"
   "emacs-git-messenger"
   "emacs-git-modes"
   "emacs-git-timemachine"
   "emacs-github-review"
   "emacs-gitlab-ci-mode"
   "emacs-gitlab-snip-helm"
   "emacs-gitpatch"
   "emacs-global-tags"
   "emacs-glsl-mode"
   "emacs-gn-mode"
   "emacs-gntp"
   "emacs-gnugo"
   "emacs-gnuplot"
   "emacs-gnus-alias"
   "emacs-gnus-harvest"
   "emacs-go-mode"
   "emacs-god-mode"
   "emacs-goggles"
   "emacs-golden-ratio"
   "emacs-google-c-style"
   "emacs-google-maps"
   "emacs-google-translate"
   "emacs-gotham-theme"
   "emacs-goto-chg"
   "emacs-gptel"
   "emacs-grandshell-theme"
   "emacs-graphql"
   "emacs-graphql-mode"
   "emacs-graphviz-dot-mode"
   "emacs-grep-a-lot"
   "emacs-grep-context"
   "emacs-groovy-modes"
   "emacs-gruber-darker"
   "emacs-gruber-darker-theme"
   "emacs-gruvbox-theme"
   "emacs-gtk-look"

   ;; Overridden by pkg:... in the $dotf/guix/home/cfg/packages/all.scm
   ;; "emacs-guix"

   "emacs-habitica"
   "emacs-hackernews"
   "emacs-handle"
   "emacs-hare-mode"
   "emacs-haskell-mode"
   "emacs-haskell-snippets"
   "emacs-hc-zenburn-theme"
   "emacs-hcl-mode"
   "emacs-heaven-and-hell"
   "emacs-helm"
   "emacs-helm-ag"
   "emacs-helm-bibtex"
   "emacs-helm-c-yasnippet"
   "emacs-helm-cider"
   "emacs-helm-cider-history"
   "emacs-helm-clojuredocs"
   "emacs-helm-company"
   "emacs-helm-css-scss"
   "emacs-helm-dash"
   "emacs-helm-descbinds"
   "emacs-helm-emms"
   "emacs-helm-eww"
   "emacs-helm-exwm"
   "emacs-helm-firefox"
   "emacs-helm-fish-completion"
   "emacs-helm-flycheck"
   "emacs-helm-gtags"
   "emacs-helm-lacarte"
   "emacs-helm-linux-disks"
   "emacs-helm-ls-git"
   "emacs-helm-lsp"
   "emacs-helm-make"
   "emacs-helm-mode-manager"
   "emacs-helm-mu"
   "emacs-helm-notmuch"
   "emacs-helm-org"
   "emacs-helm-org-contacts"
   "emacs-helm-org-rifle"
   "emacs-helm-pass"
   "emacs-helm-projectile"
   "emacs-helm-selector"
   "emacs-helm-shell-history"
   "emacs-helm-slack"
   "emacs-helm-slime"
   "emacs-helm-sly"
   "emacs-helm-switch-to-repl"
   "emacs-helm-swoop"
   "emacs-helm-system-packages"
   "emacs-helm-taskrunner"
   "emacs-helm-themes"
   "emacs-helm-wikipedia"
   "emacs-helm-wordnut"
   "emacs-helm-xref"
   "emacs-helpful"
   "emacs-hemisu-theme"
   "emacs-hercules"
   "emacs-heroku-theme"
   "emacs-hexrgb"
   "emacs-hg-histedit"
   "emacs-hgignore-mode"
   "emacs-hide-lines"
   "emacs-hide-mode-line"
   "emacs-hideshowvis"
   "emacs-highlight"
   "emacs-highlight-defined"
   "emacs-highlight-doxygen"
   "emacs-highlight-escape-sequences"
   "emacs-highlight-indent-guides"
   "emacs-highlight-indentation"
   "emacs-highlight-numbers"
   "emacs-highlight-sexp"
   "emacs-highlight-stages"
   "emacs-highlight-symbol"
   "emacs-hl-todo"
   "emacs-hledger-mode"
   "emacs-hlint-refactor"
   "emacs-hlint-refactor-mode"
   "emacs-howm"
   "emacs-hsluv"
   "emacs-ht"
   "emacs-html-to-hiccup"
   "emacs-htmlize"
   "emacs-hy-mode"
   ;; "emacs-hybrid-mode"  ;; part of spacemacs
   "emacs-hydra"
   "emacs-hyperbole"
   "emacs-hyperspace"
   "emacs-ibrowse"
   "emacs-ibuffer-projectile"
   "emacs-ibuffer-vc"
   "emacs-icomplete-vertical"
   "emacs-idle-highlight"
   "emacs-ido-at-point"
   "emacs-ido-completing-read+"
   "emacs-ido-ubiquitous"
   "emacs-ido-vertical-mode"
   "emacs-idris-mode"
   "emacs-iedit"
   "emacs-image+"
   "emacs-imenu-anywhere"
   "emacs-imenu-list"
   "emacs-inf-janet"
   "emacs-inf-ruby"
   "emacs-inflections"
   "emacs-info-plus"
   "emacs-inheritenv"
   "emacs-inkpot-theme"
   "emacs-inspector"
   "emacs-interactive-align"
   "emacs-interleave"
   "emacs-ir-black-theme"
   "emacs-irfc"
   "emacs-irony-eldoc"
   "emacs-irony-mode"
   "emacs-irony-mode-server"
   "emacs-isearch+"
   "emacs-isearch-dabbrev"
   "emacs-isearch-prop"
   "emacs-islisp-mode"
   "emacs-itail"
   "emacs-iter2"
   "emacs-ivy"
   "emacs-ivy-avy"
   "emacs-ivy-clipmenu"
   "emacs-ivy-hydra"
   "emacs-ivy-omni-org"
   "emacs-ivy-pass"
   "emacs-ivy-posframe"
   "emacs-ivy-rich"
   "emacs-ivy-taskrunner"
   "emacs-ivy-xref"
   "emacs-ivy-yasnippet"
   "emacs-jabber"
   "emacs-janet-mode"
   "emacs-jarchive"
   "emacs-jazz-theme"
   "emacs-jbeans-theme"
   "emacs-jedi"
   "emacs-jenkinsfile-mode"
   "emacs-jinja2-mode"
   "emacs-jinx"
   "emacs-jit-spell"
   "emacs-jq-mode"
   "emacs-js-comint"
   "emacs-js2-mode"
   "emacs-js2-refactor"
   "emacs-js2-refactor-el"  ;; probably not needed
   "emacs-jsdoc"
   "emacs-json-mode"
   "emacs-json-navigator"
   "emacs-json-reformat"
   "emacs-json-snatcher"
   "emacs-jsonnet-mode"
   "emacs-jsonrpc"
   "emacs-julia-mode"
   "emacs-julia-repl"
   "emacs-julia-snail"
   "emacs-jump-last"
   "emacs-jupyter"
   "emacs-kakoune"
   "emacs-kana"
   "emacs-kanji"
   "emacs-kaocha-runner"
   "emacs-kaolin-themes"
   "emacs-kbd"
   "emacs-key-chord"
   "emacs-keycast"
   "emacs-keyfreq"
   "emacs-keystore-mode"
   "emacs-kibit-helper"
   "emacs-kill-buffers"
   "emacs-kind-icon"
   "emacs-know-your-http-well"
   "emacs-kodi-remote"
   "emacs-kotlin-mode"
   "emacs-kv"
   "emacs-lacarte"
   "emacs-langtool"
   "emacs-latex-extra"
   "emacs-latex-preview-pane"
   "emacs-lcr"
   "emacs-leaf"
   "emacs-ledger-mode"
   "emacs-leetcode"
   "emacs-lemon"
   "emacs-let-alist"
   "emacs-lexic"
   "emacs-liberime"
   "emacs-libgit"
   "emacs-libmpdel"
   "emacs-libyaml"
   "emacs-lice-el"
   "emacs-ligature"
   "emacs-light-soap-theme"
   "emacs-lin"
   "emacs-lingva"
   "emacs-link-hint"
   "emacs-linum-relative"
   "emacs-lisp-extra-font-lock"
   "emacs-lispy"
   "emacs-lispyville"
   "emacs-list-utils"
   "emacs-litable"
   "emacs-literate-calc-mode"
   "emacs-literate-elisp"
   "emacs-load-relative"
   "emacs-loc-changes"
   "emacs-loccur"
   "emacs-log4e"
   "emacs-logos"
   "emacs-logview"
   "emacs-lojban"
   "emacs-loop"
   "emacs-lorem-ipsum"
   "emacs-lpy"
   "emacs-lsp-booster"
   "emacs-lsp-haskell"
   "emacs-lsp-ivy"
   "emacs-lsp-java"
   "emacs-lsp-lua-emmy"
   "emacs-lsp-metals"
   "emacs-lsp-mode"
   "emacs-lsp-python-ms"
   "emacs-lsp-treemacs"
   "emacs-lsp-ui"
   "emacs-lsp-volar"
   "emacs-lua-mode"
   "emacs-lush-theme"
   "emacs-m-buffer-el"
   "emacs-macrostep"
   "emacs-macrostep-geiser"
   "emacs-madhat2r-theme"
   "emacs-magit"
   "emacs-magit-annex"
   "emacs-magit-gerrit"
   "emacs-magit-org-todos-el"
   "emacs-magit-popup"
   "emacs-magit-svn"
   "emacs-magit-todos"
   "emacs-majapahit-themes"
   "emacs-major-mode-hydra"
   "emacs-make-it-so"
   "emacs-makey"
   "emacs-malyon"
   "emacs-map"
   "emacs-marginalia"
   "emacs-marginalia-emprise"
   "emacs-markdown-mode"
   "emacs-markdown-preview-mode"
   "emacs-markup-faces"
   "emacs-mastodon"
   "emacs-matcha"
   "emacs-material-theme"
   "emacs-math-preview"
   "emacs-math-symbol-lists"
   "emacs-matrix-client"
   "emacs-mbsync"
   "emacs-mc-extras"
   "emacs-mct"
   "emacs-md4rd"
   "emacs-memoize"
   "emacs-mentor"
   "emacs-meow"
   "emacs-meson-mode"
   "emacs-message-view-patch"
   "emacs-message-x"
   "emacs-metal-mercury-mode"
   "emacs-mew"
   "emacs-mini-echo"
   "emacs-mini-frame"
   "emacs-minibuffer-line"
   "emacs-miniedit"
   "emacs-minimal"
   "emacs-minimal-theme"
   "emacs-minimap"
   "emacs-minions"
   "emacs-minitest"
   "emacs-mint-mode"
   "emacs-mit-scheme-doc"
   "emacs-mixed-pitch"
   "emacs-mkmcc-gnuplot-mode"
   "emacs-mmm-mode"
   "emacs-mmt"
   "emacs-mocker"
   "emacs-modalka"
   "emacs-mode-line-bell"
   "emacs-mode-line-idle"
   "emacs-modus-themes"
   "emacs-moe-theme"
   "emacs-moe-theme-el"
   ;; "emacs-molokai-theme"  ;; needs emacs-color-theme
   "emacs-monky"
   "emacs-monochrome-theme"
   "emacs-monokai-theme"
   "emacs-monroe"
   "emacs-mood-line"
   "emacs-moody"
   "emacs-motif"
   "emacs-move-text"
   "emacs-mpdel"
   "emacs-mpv"
   "emacs-mu4e-alert"
   "emacs-mu4e-column-faces"
   "emacs-mu4e-dashboard"
   "emacs-mu4e-jump-to-list"
   "emacs-multi"
   "emacs-multi-term"
   "emacs-multi-vterm"
   "emacs-multifiles"
   "emacs-multiple-cursors"
   "emacs-multitran"
   "emacs-muse"
   "emacs-mustache"
   "emacs-mustang-theme"
   "emacs-mwim"
   "emacs-nadvice"
   "emacs-nameless"
   "emacs-names"
   "emacs-nano-modeline"
   "emacs-naquadah-theme"
   "emacs-nasm-mode"
   "emacs-navi-mode"
   "emacs-navigel"
   "emacs-neotree"
   "emacs-nerd-icons"
   "emacs-new-purescript-mode"
   "emacs-next"
   "emacs-next-minimal"
   "emacs-next-pgtk"
   "emacs-next-pgtk-xwidgets"
   "emacs-nginx-mode"
   "emacs-nhexl-mode"
   "emacs-nice-citation"
   "emacs-nix-mode"
   "emacs-nnreddit"
   "emacs-no-littering"
   "emacs-no-x"
   "emacs-no-x-toolkit"
   "emacs-noctilux-theme"
   "emacs-nodejs-repl"
   "emacs-noflet"
   "emacs-nominatim"
   "emacs-nord-theme"
   "emacs-notmuch"
   "emacs-notmuch-indicator"
   "emacs-notmuch-maildir"
   "emacs-nov-el"
   "emacs-npm-mode"
   "emacs-nssh"
   "emacs-nswbuff"
   "emacs-ntlm"
   "emacs-nyan-mode"
   "emacs-nyxt"
   "emacs-oauth2"
   "emacs-ob-async"
   "emacs-ob-elm"
   "emacs-ob-erlang"
   "emacs-ob-go"
   "emacs-ob-ipython"
   "emacs-ob-restclient"
   "emacs-ob-sclang"
   "emacs-objed"
   "emacs-obsidian-theme"
   "emacs-occidental-theme"
   "emacs-ol-notmuch"
   "emacs-oldlace-theme"
   "emacs-olivetti"

;;; ~/.config/emacs/bin/doom install
;;; No such file or directory /gnu/store/k8y1npgp5w7y96vrgddyx3svh2h69zxj-emacs-omtose-phellack-theme-0.2.0-0.66f9963/share/emacs/site-lisp/omtose-phellack-theme-0.2.0-0.66f9963/omtose-phellack-theme-autoloads
   ;; "emacs-omtose-phellack-theme" ;; omtose-darker-theme omtose-softer-theme

   "emacs-on"
   "emacs-on-screen"
   "emacs-opencl-mode"
   "emacs-openwith"
   "emacs-orca"
   "emacs-orderless"
   "emacs-org"
   "emacs-org-agenda-files-track"
   "emacs-org-agenda-files-track-ql"
   "emacs-org-appear"
   "emacs-org-auto-expand"
   "emacs-org-auto-tangle"
   "emacs-org-babel-eval-in-repl"
   "emacs-org-beautify-theme"
   "emacs-org-board"
   "emacs-org-books"
   "emacs-org-brain"
   "emacs-org-bullets"
   "emacs-org-caldav"
   "emacs-org-chef"
   "emacs-org-cliplink"
   "emacs-org-contacts"
   "emacs-org-contrib"
   "emacs-org-cv"
   "emacs-org-dailies"
   "emacs-org-download"
   "emacs-org-drill"
   "emacs-org-drill-table"
   "emacs-org-edit-latex"
   "emacs-org-edna"
   "emacs-org-emms"
   "emacs-org-fancy-priorities"
   "emacs-org-fc"
   "emacs-org-fragtog"
   "emacs-org-generate"
   "emacs-org-glossary"
   "emacs-org-inline-pdf"
   "emacs-org-jira"
   "emacs-org-journal"
   "emacs-org-make-toc"
   "emacs-org-mime"
   "emacs-org-mind-map"
   "emacs-org-modern"
   "emacs-org-msg"
   "emacs-org-noter"
   "emacs-org-now"
   "emacs-org-pandoc-import"
   "emacs-org-pomodoro"
   "emacs-org-present"
   "emacs-org-pretty-table"
   "emacs-org-ql"
   "emacs-org-rainbow-tags"
   "emacs-org-re-reveal"
   "emacs-org-recent-headings"
   "emacs-org-recur"
   "emacs-org-redmine"
   "emacs-org-ref"
   "emacs-org-reveal"
   "emacs-org-rich-yank"
   "emacs-org-roam"
   "emacs-org-roam-bibtex"
   "emacs-org-roam-ui"
   "emacs-org-sidebar"
   "emacs-org-static-blog"
   "emacs-org-street"
   "emacs-org-super-agenda"
   "emacs-org-superstar"
   "emacs-org-tanglesync"
   "emacs-org-transclusion"
   "emacs-org-tree-slide"
   "emacs-org-trello"
   "emacs-org-vcard"
   "emacs-org-web-tools"
   "emacs-org-webring"
   "emacs-org-wild-notifier"
   "emacs-org2web"
   "emacs-orgalist"
   "emacs-organic-green-theme"
   "emacs-orgit"
   "emacs-orglink"
   "emacs-orgmdb"
   "emacs-origami"
   "emacs-origami-el"
   "emacs-osm"
   "emacs-outorg"
   "emacs-outshine"
   "emacs-ov"
   "emacs-ox-epub"
   "emacs-ox-gemini"
   "emacs-ox-gfm"
   "emacs-ox-haunt"
   "emacs-ox-html-stable-ids"
   "emacs-ox-hugo"
   "emacs-ox-pandoc"
   "emacs-ox-reveal"
   "emacs-ox-rss"
   "emacs-ox-tufte"
   "emacs-ox-twbs"
   "emacs-pabbrev"
   "emacs-pacfiles-mode"
   "emacs-package-build"
   "emacs-package-lint"
   "emacs-packed"
   "emacs-page-break-lines"
   "emacs-pandoc-mode"
   "emacs-paradox"
   "emacs-paredit"
   "emacs-paren-face"
   "emacs-parent-mode"
   "emacs-parinfer-mode"
   "emacs-parsebib"
   "emacs-parsec"
   "emacs-parseclj"
   "emacs-parseedn"
   "emacs-pasp-mode"
   "emacs-pass"
   "emacs-password-generator"
   "emacs-password-store"
   "emacs-password-store-otp"
   "emacs-pcmpl-args"
   "emacs-pcre2el"
   "emacs-pdb-capf"
   "emacs-pddl-mode"
   "emacs-pde"
   "emacs-pdf-tools"
   "emacs-pdfgrep"
   "emacs-peep-dired"
   "emacs-peg"
   "emacs-persist"
   "emacs-persistent-scratch"
   "emacs-persp-mode"
   "emacs-perspective"
   "emacs-pfuture"
   "emacs-pg"
   "emacs-pgtk"
   "emacs-pgtk-xwidgets"
   "emacs-phi-search"
   "emacs-phi-search-mc"
   "emacs-phoenix-dark-mono-theme"
   "emacs-phoenix-dark-pink-theme"
   "emacs-php-mode"
   "emacs-picpocket"
   "emacs-piem"
   "emacs-pinentry"
   "emacs-pinyinlib"
   "emacs-pippel"
   "emacs-pkg-info"
   "emacs-placeholder"
   "emacs-planet-theme"
   "emacs-plantuml-mode"
   "emacs-platformio-mode"
   "emacs-plz"
   "emacs-poet-theme"
   "emacs-poke-mode"
   "emacs-poly-noweb"
   "emacs-poly-r"
   "emacs-polymode"
   "emacs-polymode-ansible"
   "emacs-polymode-markdown"
   "emacs-polymode-org"
   "emacs-popon"
   "emacs-popper"
   "emacs-popup"
   "emacs-popup-kill-ring"
   "emacs-popwin"
   "emacs-pos-tip"
   "emacs-posframe"
   "emacs-powerline"
   "emacs-powershell"
   "emacs-prescient"
   "emacs-prettier"
   "emacs-pretty-hydra"
   "emacs-pretty-mode"
   "emacs-pretty-speedbar"
   "emacs-prism"
   "emacs-prodigy"
   "emacs-professional-theme"
   "emacs-project"
   "emacs-project-mode-line-tag"
   "emacs-project-tab-groups"
   "emacs-project-x"
   "emacs-projectile"
   "emacs-promise"
   "emacs-prop-menu"
   "emacs-protobuf-mode"
   "emacs-psc-ide"
   "emacs-psession"
   "emacs-pubmed"
   "emacs-pug-mode"
   "emacs-pulsar"
   "emacs-pulseaudio-control"
   "emacs-puni"
   "emacs-punpun-theme"
   "emacs-puppet-mode"
   "emacs-purescript-mode"
   "emacs-purple-haze-theme"
   "emacs-purs-mode"
   "emacs-py-isort"
   "emacs-pydoc"
   "emacs-pyim"
   "emacs-pyim-basedict"
   "emacs-pyimport"
   "emacs-python-black"
   "emacs-python-environment"
   ;; "emacs-pythonic"    ;; fails to compile
   "emacs-pyvenv"
   "emacs-qml-mode"
   "emacs-qrencode"
   "emacs-quasi-monochrome"
   "emacs-queue"
   "emacs-quickrun"
   "emacs-racer"
   "emacs-racket-mode"
   "emacs-rail"
   "emacs-railscasts-theme"
   "emacs-rainbow-blocks"
   "emacs-rainbow-delimiters"
   "emacs-rainbow-identifiers"
   "emacs-rainbow-mode"
   "emacs-read-only-cfg"
   "emacs-realgud"
   "emacs-rebecca-theme"
   "emacs-rec-mode"
   "emacs-recent-addresses"
   "emacs-redshank"
   "emacs-refactor"
   "emacs-reformatter"
   "emacs-relative-buffers"
   "emacs-relint"
   "emacs-repl-toggle"
   "emacs-repo"
   "emacs-repology"
   "emacs-request"
   "emacs-restart-emacs"
   "emacs-restclient"
   "emacs-reverse-im"
   "emacs-reverse-theme"
   "emacs-rfcview"
   "emacs-rg"
   "emacs-rich-minority"
   "emacs-rime"
   "emacs-ripgrep"
   "emacs-riscv-mode"
   "emacs-rjsx-mode"
   "emacs-rmsbolt"
   "emacs-robe"
   "emacs-robot-log"
   "emacs-robot-mode"
   "emacs-robots-txt-mode"
   "emacs-rocket-chat"
   "emacs-roguel-ike"
   "emacs-rotate-text"
   "emacs-rpm-spec-mode"
   "emacs-rspec"
   "emacs-rsw-elisp"
   "emacs-rudel"
   "emacs-rust-mode"
   "emacs-rustic"
   "emacs-ryo-modal"
   "emacs-s"
   "emacs-sakura-theme"
   "emacs-saveplace-pdf-view"
   "emacs-sayid"
   "emacs-sbt-mode"
   "emacs-scad-mode"
   "emacs-scala-mode"
   "emacs-scel"
   "emacs-scheme-complete"
   "emacs-scpaste"
   "emacs-scratch-el"
   "emacs-scribble-mode"
   "emacs-scroll-on-drag"
   "emacs-sdcv"
   "emacs-seeing-is-believing"
   "emacs-selectrum"
   "emacs-semantic-refactor"
   "emacs-semi-epg"
   "emacs-seq"
   "emacs-seriestracker"
   "emacs-sesman"
   "emacs-seti-theme"
   "emacs-setup"
   "emacs-shackle"
   "emacs-shell-command+"
   "emacs-shell-here"
   "emacs-shell-pop"
   "emacs-shell-switcher"
   "emacs-shift-number"
   "emacs-showtip"
   "emacs-shrink-path"
   "emacs-shroud"
   "emacs-shut-up"
   "emacs-shx"
   "emacs-simple-httpd"
   "emacs-simple-modeline"
   "emacs-simple-mpc"
   "emacs-skeletor"
   "emacs-skempo"
   "emacs-skewer-mode"
   "emacs-slack"
   "emacs-slim-mode"
   "emacs-slime"
   "emacs-slime-company"
   "emacs-slime-repl-ansi-color"
   "emacs-slime-volleyball"
   "emacs-slite"
   "emacs-sly"
   "emacs-sly-asdf"
   "emacs-sly-macrostep"
   "emacs-sly-named-readtables"
   "emacs-sly-package-inferred"
   "emacs-sly-quicklisp"
   "emacs-sly-stepper"
   "emacs-smart-hungry-delete"
   "emacs-smart-mode-line"
   "emacs-smartparens"
   "emacs-smex"
   "emacs-sml-mode"
   "emacs-smyx-theme"
   "emacs-snow"
   "emacs-so-long"
   "emacs-soap-client"
   "emacs-soft-charcoal-theme"
   "emacs-soft-morning-theme"
   "emacs-soft-stone-theme"
   "emacs-solaire-mode"
   "emacs-solarized-theme"
   "emacs-solidity"
   "emacs-soothe-theme"
   "emacs-sourcemap"
   "emacs-spacegray-theme"
   "emacs-spaceleader"
   "emacs-spaceline"
   "emacs-spaceline-all-the-icons"
   "emacs-spaceline-next"
   "emacs-spacemacs"
   "emacs-spacemacs-theme"
   "emacs-spamfilter-el"
   "emacs-spark"
   "emacs-sparql-mode"
   "emacs-speed-type"
   "emacs-spell-fu"
   "emacs-sphinx-doc"
   "emacs-spinner"
   "emacs-spongebob"
   "emacs-spray"
   "emacs-springboard"
   "emacs-sql-indent"
   "emacs-sqlite"
   "emacs-sqlite3-api"
   "emacs-sr-speedbar"
   "emacs-srfi"
   "emacs-srht"
   "emacs-srv"
   "emacs-ssh-agency"
   "emacs-ssh-config-mode"
   "emacs-standard-dirs"
   "emacs-starlit-theme"
   "emacs-stickyfunc-enhance"
   "emacs-strace-mode"
   "emacs-straight-el"
   "emacs-stream"
   "emacs-string-inflection"
   "emacs-stripe-buffer"
   "emacs-stumpwm-mode"
   "emacs-subatomic-theme"
   "emacs-subatomic256-theme"
   "emacs-subed"
   "emacs-sublime-themes"
   "emacs-substitute"
   "emacs-sudo-edit"
   "emacs-suggest"
   "emacs-suneater-theme"
   "emacs-sunny-day-theme"
   "emacs-super-save"
   "emacs-svg-icon"
   "emacs-svg-lib"
   "emacs-svg-tag-mode"
   "emacs-sway"
   "emacs-sweet-theme"
   "emacs-swiper"
   "emacs-switch-buffer-functions"
   "emacs-switch-window"
   "emacs-sx"
   "emacs-sxiv"
   "emacs-symbol-overlay"
   "emacs-symon"
   "emacs-synosaurus"
   "emacs-syslog-mode"
   "emacs-systemd-mode"
   "emacs-tablist"
   ;; "emacs-tagedit" ;; fails to compile
   "emacs-tamil99"
   "emacs-tango-2-theme"
   "emacs-tango-plus-theme"
   "emacs-tangotango-theme"
   "emacs-tao-theme"
   "emacs-taskrunner"
   "emacs-taxy"
   "emacs-taxy-magit-section"
   "emacs-tco-el"
   "emacs-telega"
   "emacs-telega-contrib"
   "emacs-telega-server"
   "emacs-telephone-line"
   "emacs-tempel"
   "emacs-tempel-collection"
   "emacs-templatel"
   "emacs-terminal-here"
   "emacs-terraform-mode"
   "emacs-test-simple"
   "emacs-theme-magic"
   "emacs-theme-sorcery"
   "emacs-tide"
   "emacs-tintin-mode"
   "emacs-tiny"
   "emacs-tl1-mode"
   "emacs-tldr"
   "emacs-tmr"
   "emacs-toc-org"
   "emacs-tokei"
   "emacs-tomelr"
   "emacs-toml-mode"
   "emacs-toodoo"
   "emacs-totp"
   "emacs-toxi-theme"
   "emacs-tracking"
   "emacs-tramp"
   "emacs-tramp-auto-auth"
   "emacs-transient"
   "emacs-transient-posframe"
   "emacs-transmission"
   "emacs-transpose-frame"
   "emacs-transpose-mark"
   "emacs-trashed"
   "emacs-tree-inspector"
   "emacs-tree-mode"
   "emacs-treebundel"
   "emacs-treemacs"
   "emacs-treemacs-extra"
   "emacs-treepy"
   "emacs-treeview"
   "emacs-ts"
   "emacs-tshell"
   "emacs-tuareg"
   "emacs-tup-mode"
   "emacs-tweaks"
   "emacs-twilight-anti-bright-theme"
   "emacs-twilight-bright-theme"
   "emacs-twilight-theme"
   "emacs-twittering-mode"
   "emacs-typescript-mode"
   "emacs-typing"
   "emacs-typit"
   "emacs-typo"
   "emacs-ujelly-theme"
   "emacs-ukrainian-holidays"
   "emacs-uml-mode"
   "emacs-undercover"
   "emacs-underwater-theme"
   "emacs-undo-fu"
   "emacs-undo-fu-session"
   "emacs-undo-propose-el"
   "emacs-undo-tree"
   "emacs-undohist-el"
   "emacs-unfill"
   "emacs-unicode-fonts"
   "emacs-unidecode"
   "emacs-unkillable-scratch"
   "emacs-unpackaged-el"
   "emacs-url-http-ntlm"
   "emacs-url-http-oauth"
   "emacs-url-scgi"
   "emacs-use-package"
   "emacs-vala-mode"
   "emacs-validate"
   "emacs-validate-html"
   "emacs-valign"
   "emacs-vc-hgcmd"
   "emacs-vcard-mode"
   "emacs-vcsh"
   "emacs-vdiff"
   "emacs-vdiff-magit"
   "emacs-vertico"
   "emacs-vertico-posframe"
   "emacs-vhdl-mode"
   "emacs-vi-tilde-fringe"
   "emacs-vimrc-mode"
   "emacs-visual-fill-column"
   "emacs-visual-regexp"
   "emacs-vlf"
   "emacs-volatile-highlights"
   "emacs-vscode-dark-plus"
   "emacs-vterm"
   "emacs-vterm-toggle"
   "emacs-vundo"
   "emacs-w3m"
   "emacs-wanderlust"
   "emacs-waveform"
   "emacs-wc-mode"
   "emacs-web-beautify"
   "emacs-web-completion-data"
   "emacs-web-mode"
   "emacs-web-server"
   "emacs-webfeeder"
   "emacs-webpaste"
   "emacs-websocket"
   "emacs-weyland-yutani-theme"
   "emacs-wget"
   "emacs-wgrep"
   "emacs-which-key"
   "emacs-which-key-posframe"
   "emacs-white-sand-theme"
   "emacs-whitespace-cleanup-mode"
   "emacs-wide-int"
   "emacs-window-layout"
   "emacs-window-purpose"
   "emacs-windower"
   "emacs-windsize"
   "emacs-wisp-mode"
   "emacs-with-editor"
   "emacs-with-simulated-input"
   "emacs-wordgen"
   "emacs-wordnut"
   "emacs-writegood-mode"
   "emacs-writeroom"
   "emacs-writeroom-mode"
   "emacs-ws-butler"
   "emacs-wttrin"
   "emacs-wucuo"
   "emacs-x509-mode"
   "emacs-xah-fly-keys"
   "emacs-xclip"
   "emacs-xcscope"
   "emacs-xelb"
   "emacs-xelb-no-x-toolkit"
   "emacs-xml-rpc"
   "emacs-xmlgen"
   "emacs-xonsh-mode"
   "emacs-xpm"
   "emacs-xr"
   "emacs-xref"
   "emacs-xterm-color"
   "emacs-xtest"
   "emacs-xwidgets"
   "emacs-yaml"
   "emacs-yaml-mode"
   "emacs-yasnippet"
   "emacs-yasnippet-snippets"
   "emacs-yeetube"
   "emacs-youtube-dl"
   "emacs-ytdl"
   "emacs-ytel"
   "emacs-ytel-show"
   "emacs-zen-and-art-theme"
   "emacs-zenburn-theme"
   "emacs-zeno-theme"
   "emacs-zerodark-theme"
   "emacs-zig-mode"
   "emacs-zmq"
   "emacs-znc"
   "emacs-zones"
   "emacs-zonokai-emacs"
   "emacs-zop-to-char"
   "emacs-zotxt"
   "emacs-zoutline"
   "emacs-ztree"
   ))
(testsymb 'guix-package---list-available)

;;; found fresh local cache at /home/bost/.cache/guile/ccache/3.0-LE-8-4.6/home/bost/dev/guix/gnu/packages.scm.go
(define (found-packages)
  ((comp
    ;; length
    (partial map package-name)
    flatten
    (partial remove null?)
    (partial map find-packages-by-name)
    )
   (guix-package---list-available)))
(testsymb 'found-packages)

(define-public (available-packages)
  (sx (guix-package---list-available) (found-packages)))
(testsymb 'available-packages)

(module-evaluated)
