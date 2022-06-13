;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path
   `(,(concat (getenv "dotf") "/emacs/" ))

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     nginx
     pdf
     (auto-completion :variables
                      ;; (setq
                      auto-completion-enable-help-tooltip t
                      ;; auto-completion-enable-help-tooltip 'manual
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      ;; )
                      )
     better-defaults
     ;; graphviz ;; for erc-social-graph-draw
     ;; TODO try out ibuffer
     ;; (ibuffer :variables ibuffer-group-buffers-by 'projects)

     ;; M-x erc/default-servers `SPC a c i D`
     (erc :variables
          erc-fill-column 120
          ;; erc-fill-function 'erc-fill-variable
          erc-fill-function 'erc-fill-static
          erc-fill-static-center 15
          ;; erc-enable-notifications nil
          erc-autojoin-channels-alist
          '(("libera.chat" "#guix"
             ;; "#systemcrafters"
             ))
          erc-prompt-for-nickserv-password nil
          erc-server-list
          '(("irc.libera.chat" :port "6667" :nick "bost" :password "")))
     php
     ;; typescript
     (typescript :variables
                 typescript-fmt-tool 'prettier
                 typescript-linter 'eslint
                 ;; typescript-fmt-on-save t
                 )
     (javascript :variables
                 javascript-fmt-tool 'prettier
                 node-add-modules-path t)

     ;; asciidoc
     emacs-lisp
     (git :variables
          ;; TODO implement it as spacemacs|toggle
          ;; (setq
          ;; git-magit-status-fullscreen t
          ;; magit-diff-refine-hunk 'all
          magit-diff-refine-hunk nil ; default value
          ;; magit-diff-refine-hunk t
          ;; )
          )

     ;;;; ivy ;; helm replacement
     (helm :variables
           ;; (setq
           ;; helm-display-function 'helm-display-buffer-in-own-frame

           ;; default 20
           ;; nil - use the longest ‘buffer-name’ length found
           helm-buffer-max-length nil

           ;; TODO helm-display-buffer-width
           ;; default
           ;; helm-display-function 'helm-default-display-buffer
           ;; )
           )

     ;; multiple-cursors

     ;; language server protocol
     ;; https://emacs-lsp.github.io/lsp-mode/
     (lsp
      :variables
      ;; (setq
      ;; lsp-enable-file-watchers nil
      ;; lsp-file-watch-threshold 1500
      ;; lsp-keymap-prefix "C-M-s-l"
      ;; lsp-enable-which-key-integration t

      ;; Indent regions using the file formatting functionality provided by the
      ;; language server. Set to nil to use CIDER features instead of LSP UI
      lsp-enable-indentation nil

      ;; Enable textDocument/onTypeFormatting integration
      lsp-enable-on-type-formatting nil

      ;; lsp-file-watch-ignored-director

      ;; docstring popup - delay in seconds for mouse and cursor
      lsp-ui-doc-delay 2

      ;; show code actions and diagnostics text as right-hand side of buffer
      lsp-ui-sideline-enable nil

      ;; show reference count for functions (assume more lenses added in future)
      lsp-lens-enable t
      ;; )
      )

     markdown
     ;; swift
     ;; windows-scripts
     (org :variables
          org-roam-v2-ack t ; switch off the ' Org-roam v2!' warning
          org-enable-roam-support t
          ;; capture content from external applications such as the browser
          ;; org-enable-roam-protocol t
          )
     ;; rust

     ;; https://gist.github.com/soegaard/942a3074513655292816e0b79c466620
     racket ;; see https://racket-mode.com

     (scheme :variables ; M-x run-guile
             scheme-implementations '(guile))
     shell-scripts
     haskell
     csv
     ;; Breaks the `C-h k command'
     ;; Error message is "mapcar: Symbol’s value as variable is void: code-cells-mode""
     ;; See https://github.com/syl20bnr/spacemacs/issues/15548
     ;; (python
     ;;  :variables
     ;;  ;; TODO use a list of prefered python interpreters
     ;;  python-shell-interpreter "python3.9.9"; python3.8 python3.7 python3.6
     ;;  ;; -i   : inspect interactively after running script; forces a prompt even
     ;;  ;; if stdin does not appear to be a terminal; also PYTHONINSPECT=x
     ;;  python-shell-interpreter-args "-i")

     ;; Show commands as you type in a separate buffer
     ;; command-log  ;; deprecated

     colors

     ;; (java :variables
     ;;       eclim-eclipse-dirs "~/eclipse-java-neon"
     ;;       eclim-executable "~/eclipse-java-neon/eclim")
     html
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)

     ;; requires:
     ;; sudo apt install --yes aspell-en aspell-fr aspell-de aspell-sk
     spell-checking  ;; SPC S / M-x flyspell-mode

     syntax-checking
     treemacs
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)
     (clojure
      ;; cider package location configured by
      ;; ~/.emacs.d/layers/+lang/clojure/packages.el in its 'use-package'
      :variables
      cider-jdk-src-paths
      '(
        ;; (concat (getenv "HOME") "/dev/clojure")
        ;; sudo apt install openjdk-15-source
        ;; mkdir -p ~/dec/openjdk-15-source
        ;; unzip /usr/lib/jvm/openjdk-15/src.zip -d ~/dec/openjdk-15-source
        "~/dec/openjdk-15-source")

      ;; cider-font-lock-dynamically '(macro core function var) ;; default '(macro core deprecated)

      cider-overlays-use-font-lock t                            ;; default undef

      cider-preferred-build-tool 'clojure-cli                   ;; default nil
      ;; cider-repl-buffer-size-limit 500                          ;; default nil; what's the unit?
      cider-repl-use-pretty-printing t                          ;; default undef

      ;; run Cider without any LSP features
      clojure-backend 'cider                                    ;; default nil

      clojure-enable-clj-refactor t                             ;; default nil
      cljr-warn-on-eval nil                                     ;; default t

      clojure-enable-linters 'clj-kondo

      ;; debugger & profiler. Default nil
      clojure-enable-sayid t

      clojure-toplevel-inside-comment-form t

      ;; Indentation of function forms
      ;; https://github.com/clojure-emacs/clojure-mode#indentation-of-function-forms
      ;; (setq clojure-indent-style 'align-arguments)

      ;; Vertically align s-expressions
      ;; https://github.com/clojure-emacs/clojure-mode#vertical-alignment
      ;; (setq clojure-align-forms-automatically t)

      ;; Auto-indent code automatically
      ;; https://emacsredux.com/blog/2016/02/07/auto-indent-your-code-with-aggressive-indent-mode/
      ;; (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
      )

     ;; (setq org-babel-clojure-backend 'cider)
     ;; (setq gui-elements 1) ; because of CIDER menu
     ;; (define-key cider-repl-mode-map "<s-delete>" nil)
     ;; (unbind-key "<s-delete>" cider-repl-mode-map)

     java

     ;; see https://github.com/syl20bnr/spacemacs/issues/12462
     ;; try also:
     ;; dotspacemacs-excluded-packages '(ensime)
     (scala :variables scala-backend 'scala-metals)

     sql
     vimscript
     javascript
     shell
     ;; command-log - log keystrokes
     search-engine
     ;; TODO eyebrowse - window management
     ;; TODO spacemacs-layout - window management
     ;; smex ; smart M-x enhacements - recent & most used commands
     ;; gnus
     ;; systemd
     ;; hy ;; hylang - lisp embedded in python
     ;; go
     ;; yaml
     ;; docker

     (latex
      ;; :variables
      ;; latex-build-command "LaTeX" ;; defaults to "LatexMk"
      ;; latex-enable-folding t      ;; defaults to nil
      ;; latex-enable-auto-fill nil  ;; defaults to t
      ;; latex-enable-magic t        ;; defaults to nil
      )
     my ;; see dotspacemacs-configuration-layer-path

     ;; (vinegar :variables                     ;; simplify dired
     ;;          vinegar-reuse-dired-buffer t
     ;;          vinegar-dired-hide-details nil
     ;;          )

     themes-megapack

     ;; dired alternative
     ;; (ranger :variables
     ;;         ;; (setq
     ;;          ranger-override-dired 'ranger
     ;;          ranger-show-dotfiles t
     ;;          ;; ranger-show-preview t
     ;;          ranger-show-hidden t
     ;;          ranger-cleanup-eagerly t
     ;;          ranger-cleanup-on-disable t
     ;;          ranger-ignored-extensions '("mkv" "flv" "iso" "mp4")
     ;;          ;; )
     ;;         )
     yaml
     )

   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   ;;
   ;; Note:
   ;; '(your-package :location local) requires '(require 'your-package) or
   ;; '(use-package your-package ...) in the `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     yasnippet-snippets

     ;; ;; JSX major mode. JSX is an XML-like syntax extension to ECMAScript
     ;; rjsx-mode

     ;; ;; Minor mode to format JS code on file save
     ;; prettier-js

     ;; (yasnippet :location ;; local
     ;;            (recipe :fetcher github :repo "Bost/yasnippet"
     ;;                    ;; :min-version "1"
     ;;                    ))
     ;; send files marked in dired via MTP to Android
     ;; dired-mtp     ; not found
     ;; android-mode  ; doesn't work
     beacon ;; Never lose your cursor again - see also 'Highlight current line'
     use-package-chords
     suggest ;; discover elisp fns
     crux
     super-save ;; save buffers when they lose focus
     zop-to-char
     fish-mode
     transpose-frame
     ;; google-this
     cider-hydra ;; pop-up menus of commands with common prefixes for CIDER

     ;; Emacs mode for the Lean theorem prover.
     ;; lean-mode

     evil-vimish-fold

     ;; crosshairs-mode messes up with the background color of the current-line
     ;; (crosshairs    :location local)
     ;; (hl-line+      :location local)
     ;; (vline         :location local)
     ;; (col-highlight :location local)

     ;; cobol-mode

     ;; org-mode-babel packages {{{
     ;; see also org-babel-load-languages
     (ob-racket
      :location (recipe :fetcher github :repo "hasu/emacs-ob-racket"))
     ;; TODO add scribble-mode pollen-mode to the racket layer; with :defer t
     ;; (defun racket/init-pollen-mode () (use-package pollen-mode :defer t))
     ;; (defun racket/init-scribble-mode () (use-package scribble-mode :defer t))
     scribble-mode
     pollen-mode
     ;; }}}

     ;; helm-lean ; Emacs mode for the Lean theorem prover.
     helm-cider-history
     helm-system-packages
     ;; helm-descbinds
     ;; helm-slime
     helm-dictionary ;; look up words in dictionaries

     ;; telegram client for emacs
     ;; TODO document the ln -s `which gcc` ~/bin/cc
     ;; telega
     ;; Error: Package fonts-symbola is unavailable
     ;; fonts-symbola ;; for the telega
     ;; Fonts installed using:
     ;;   guix package -i font-gnu-{freefont,unifont}

     ;; https://gitter.im/syl20bnr/spacemacs?at=5dba1a66e886fb5aa225bcf8
     ;; dired-x is part of the spacemacs-defaults layer, so it's used by default.
     ;; It extends dired by:
     ;;     Omitting uninteresting files
     ;;     Guessing shell commands
     ;;     Running Dired command in non-Dired
     ;;     Finding a file mentioned in a buffer
     ;;     Commands using file marking
     ;; dired+ is unavailable
     )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '((farmhouse-light-mod :location local)
                         material
                         misterioso
                         spacemacs-light
                         spacemacs-dark
                         twilight-anti-bright
                         underwater
                         solarized-dark-high-contrast
                         heroku)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.0)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 10.0
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup t

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   ;; can't zoom-in if it's `t'; use `display-line-numbers-mode'
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   ;; use `origami' for folding of:
   ;; <description> {{{
   ;;     <some content>
   ;; }}}
   ;; TODO use spacemacs|toggle for dotspacemacs-folding-method
   ;; (setq
   dotspacemacs-folding-method 'evil
   ;; dotspacemacs-folding-method 'origami
   ;; dotspacemacs-folding-method 'vimish
   ;; )

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
)

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  (progn
    (setq
     ;; Avoid creation of dotspacemacs/emacs-custom-settings
     ;; https://github.com/syl20bnr/spacemacs/issues/7891
     custom-file "~/.emacs.d/.cache/.custom-settings")
    (let ((ret-val (load custom-file)))
      (message "custom-file loaded. ret-val %s" ret-val)))

  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/"))
  (add-to-list 'package-pinned-packages '(telega . "melpa-stable"))
  )


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
)


(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; https://gist.github.com/synic/5c1a494eaad1406c5519
  ;; (defvar ao/v-dired-omit t
  ;;   "If dired-omit-mode enabled by default. Don't setq me.")

  ;; (defun ao/dired-omit-switch ()
  ;;   "This function is a small enhancement for `dired-omit-mode', which will
  ;;  \"remember\" omit state across Dired buffers."
  ;;   (interactive)
  ;;   (if (eq ao/v-dired-omit t)
  ;;       (setq ao/v-dired-omit nil)
  ;;     (setq ao/v-dired-omit t))
  ;;   (ao/dired-omit-caller)
  ;;   (when (equal major-mode 'dired-mode)
  ;;     (revert-buffer)))

  ;; (defun ao/dired-omit-caller ()
  ;;   (if ao/v-dired-omit
  ;;       (setq dired-omit-mode t)
  ;;     (setq dired-omit-mode nil)))

  ;; (defun ao/dired-back-to-top()
  ;;   "Move to the first file."
  ;;   (interactive)
  ;;   (beginning-of-buffer)
  ;;   (dired-next-line 2))

  ;; (defun ao/dired-jump-to-bottom()
  ;;   "Move to last file."
  ;;   (interactive)
  ;;   (end-of-buffer)
  ;;   (dired-next-line -1))

  ;; Dired
  ;; ;; dired-x is part of the spacemacs-defaults layer, so it's used by default.
  ;; (require 'dired-x) ; Enable dired-x
  ;; ;; dired+ is unavailable
  ;; ;; (require 'dired+)  ; Enable dired+
  ;; (setq-default dired-omit-files-p t)  ; Don't show hidden files by default
  ;; (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$\\|\\.pyc$"))
  ;; (add-hook 'dired-mode-hook 'ao/dired-omit-caller)
  ;; (define-key evil-normal-state-map (kbd "_") 'projectile-dired)
  ;; (define-key evil-normal-state-map (kbd "-") 'dired-jump)
  ;; (setq diredp-hide-details-initially-flag nil)
  ;; (advice-add 'spacemacs/find-dotfile :around 'ao/find-dotfile)
  ;; ;; Make `gg' and `G' do the correct thing
  ;; (eval-after-load "dired-mode"
  ;;   (evilified-state-evilify
  ;;    ;; evilify
  ;;    dired-mode dired-mode-map
  ;;            "f" 'helm-find-files
  ;;            "h" 'diredp-up-directory-reuse-dir-buffer
  ;;            "l" 'diredp-find-file-reuse-dir-buffer
  ;;            "I" 'ao/dired-omit-switch
  ;;            "gg" 'ao/dired-back-to-top
  ;;            "G" 'ao/dired-jump-to-bottom))

  ;; typescript-indent-level is overridden by project-specific .editorconfig
  ;; (setq-default typescript-indent-level 4)

  ;; (sp-use-paredit-bindings)

  ;; set up `evil' bindings for `info-mode'
  (evil-collection-info-setup)
  ;; ... or try to remove evil support in the Info buffers (doesn't work)
  ;; (remove-hook 'Info-mode-hook 'evil-mode)

  ;; tide - typescript IDE def func:
  (defun tide-setup-hook ()
    (tide-setup)
    (eldoc-mode)
    (tide-hl-identifier-mode +1)
    (setq web-mode-enable-auto-quoting nil)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-attr-indent-offset 2)
    (setq web-mode-attr-value-indent-offset 2)
    (setq lsp-eslint-server-command
          `("node"
            ,(concat
              (getenv "HOME")
              "/.emacs.d/.cache/lsp/eslint/unzipped/extension/server/out/eslintServer.js")
            "--stdio"))
    (set (make-local-variable 'company-backends)
         '((company-tide company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))

  (add-hook 'before-save-hook 'tide-format-before-save)

  ;; use rjsx-mode for .js* files except json and use tide with rjsx
  (add-to-list 'auto-mode-alist '("\\.js.*$" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
  (add-hook 'rjsx-mode-hook 'tide-setup-hook)

  ;; web-mode extra config
  (add-hook 'web-mode-hook 'tide-setup-hook
            (lambda () (pcase (file-name-extension buffer-file-name)
                         ("tsx" ('tide-setup-hook))
                         (_ (my-web-mode-hook)))))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (add-hook 'web-mode-hook 'company-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook #'turn-on-smartparens-mode t)

  (add-to-list 'yas-snippet-dirs
               (concat
                (getenv "HOME")
                "/.emacs.d/layers/+completion/auto-completion/local/snippets/"))
  ;; the (add-to-list 'yas-snippet-dirs ...) must be called before
  (yas-global-mode 1)
  (global-flycheck-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode)

  ;; Company is a modular completion framework.
  (global-company-mode)

  (add-to-list 'auto-mode-alist '("\\.cob" . cobol-mode))

  (add-to-list 'exec-path "/usr/local/bin") ;; for cider on guix

  ;; (spacemacs/toggle-menu-bar-on)
  ;; (global-prettify-symbols-mode +1)

  (setq
   ;; See also undo-tree-auto-save-history
   undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))

   ;; TODO create toggle for evil-ex-substitute-interactive-replace
   evil-ex-substitute-interactive-replace t ;; nil/t. default is t

   ;; Kill process buffer without confirmation
   ;; See https://emacs.stackexchange.com/a/14511
   kill-buffer-query-functions (delq 'process-kill-buffer-query-function
                                     kill-buffer-query-functions)

   ;; See https://emacs.stackexchange.com/q/22283 and
   ;; `ls-lisp-use-insert-directory-program', `ls-lisp-dirs-first'
   dired-listing-switches "--group-directories-first --dereference -al"
   ;; dired-listing-switches "--group-directories-first -al"

   ;; none of these works; not even in the `dotspacemacs/user-init'
   ;; has to be set as `custom-set-variables'
   ;; display-line-numbers t
   ;; global-display-line-numbers-mode t
   ;; display-line-numbers 'relative
   ;; display-line-numbers 'visual

   ;; prevent: Error saving to X clipboard manager
   x-select-enable-clipboard-manager nil

   goto-address-mode nil
   frame-title-format "%f - Emacs" ; 'path/to/file' in title bar; %b only 'file'
   bookmark-default-file (concat (getenv "dotf") "/emacs/bookmarks")
   ;; Hotfix of "magit ediff on unstaged file leads to emacs freeze. #4730"
   ediff-window-setup-function 'ediff-setup-windows-default

   ;; Fix projectile-regenerate-tags: ctags: invalid option -- ’e’
   ;; See https://github.com/bbatsov/projectile/issues/133
   projectile-tags-command "ctags-exuberant -Re -f \"%s\" %s"

   create-lockfiles nil ;; do not create .# lockfiles
   vc-follow-symlinks t ;; auto follow symbolic links
   ;; on GuixOS browse-url-firefox-program evaluates to "icecat" by default
   browse-url-firefox-program "firefox"
   browse-url-browser-function 'browse-url-default-browser

   font-latex-fontify-script nil
   org-latex-listings 'minted
   org-latex-packages-alist '(("" "minted"))
   ;; (setq
   org-latex-pdf-process ;; is it customizable? customize-set-variable
   '("%latex -interaction nonstopmode -output-directory %o %f"
     "%latex -interaction nonstopmode -output-directory %o %f"
     "%latex -interaction nonstopmode -output-directory %o %f")
   ;; '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   ;;   "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
   ;; )


   ;; color-identifiers-mode t
   )

  (setq-default
   ;; Truncate lines in every buffer. Overridden by
   ;; `truncate-partial-width-windows'
   ;; See also: ;; (setq-default global-visual-line-mode t)
   ;; Using `setq-default' makes it default for all buffers.
   truncate-lines t)

  (blink-cursor-mode t)

  (progn
    (setq
     global-hl-line-sticky-flag t)
    (global-hl-line-mode +1))

  (beacon-mode 1)

  (progn
    (setq
     ;; Max time delay between two key presses to be considered a key chord.
     ;; default 0.1
     key-chord-two-keys-delay 0.02)
    (key-chord-mode 1))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (R . t)
     (latex . t)
     (emacs-lisp . t)
     (C . t)
     (scheme . t)
     (racket . t)
     ;; Although adviced by https://github.com/hasu/emacs-ob-racket this leads
     ;; to 'ob-scribble not found' when activated:
     ;; (scribble . t)
     (python . t)
     ;; (ditaa . t)
     (clojure . t)
     (java . t)
     (shell . t)
     ;; (plantuml . t)
     (js . t)
     ;; (kotlin . t)
     (lisp . t)
     ;; (ruby . t)
     (haskell . t)
     ))

  (defalias 'save-selected-text 'write-region)

  (defun my=load-layout ()
    "docstring"
    (interactive)
    (persp-load-state-from-file "~/.emacs.d/.cache/layouts/persp-auto-save")
    )

  (defun my=delete-other-windows ()
    "docstring"
    (interactive)
    ;; See definitions of `treemacs'
    (pcase (treemacs-current-visibility)
      ('visible (delete-window (treemacs-get-local-window)))
      ;; ('exists  (treemacs-select-window))
      ;; ('none    (treemacs--init))
      )
    (delete-other-windows)
    )
  ;; disable mouse support in X11 terminals - enables copy/paste with mouse
  ;; (xterm-mouse-mode -1)
  (super-save-mode +1) ;; better auto-save-mode

  (use-package org
    :config
    (setq
     org-support-shift-select 'always
     org-src-tab-acts-natively nil ;; default is t
     )
    :hook
    (org-mode
     .
     (lambda ()
       "Don't increase the height relative to the other text."
       (dolist (face
                '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5))
         (set-face-attribute face nil
                             :weight 'bold ; 'semi-bold
                             :height 1.0))))
    )

  (use-package fish-mode :hook (fish-mode . paredit-mode))

  (use-package emacs
    :hook (emacs-lisp-mode
           .
           (lambda () ;; capital lambda char Λ
             (push '("my=interactive-lambda" . 923) prettify-symbols-alist))))

  (use-package clojure-mode) ;; must be here for the bind-keys
  ;; '(use-package cider ...)' must be here for the bind-keys
  (use-package cider)

  ;; TODO the ~M-`~ (M-x tmm-menubar) can be used for other purposes

  ;; TODO my=eval-bind-keys-and-chords
  ;; ~SPC m e c~ or M-x spacemacs/eval-current-form-sp

  ;; TODO autoload
  (spacemacs/declare-prefix "og" "google-this")
  (spacemacs/set-leader-keys
    "ogg" 'google-this
    "ogr" 'google-this-region
    "oc"  'my=cider-clear-compilation-highlights
    ;; "oc"  'org-roam-capture
    ;; "of"  'my=switch-to-repl-start-figwheel
    "or"  'rotate-frame
    ;; Show list of references to a given node from other nodes
    "ob"  'org-roam-buffer-toggle
    "of"  'org-roam-node-find
    "oi"  'org-roam-node-insert
    "op"  'my=yank-and-select
    ;; These two functions seem not to be useful:
    ;; "op" 'my=paste-from-clipboard ; TODO is this function useful?
    ;; "oy" 'my=copy-to-clipboard    ; TODO is this function useful?
    )

  (dolist (mode `(clojure-mode
                  clojure-modec
                  clojurescript-mode
                  cider-repl-mode))
    (spacemacs/set-leader-keys-for-major-mode mode
      "f" 'my=switch-to-repl-start-figwheel
      "c" 'my=cider-clear-compilation-highlights))

  (spacemacs|add-cycle
      defun-narrow-modes
    '(narrow-to-defun narrow-to-page narrow-to-region widen)
    ;; :evil-leader "tnn"
    :documentation "Cycle through the narrow ? modes ?")

  (spacemacs|add-cycle
      narrow-widen
    '(narrow-to-defun widen)
    ;; :evil-leader "tnn"
    :documentation "Toggle between `narrow-to-defun' and `widen'")

  (spacemacs|add-cycle
      large-file-settings
    '(my=shenanigans-on my=shenanigans-off)
    my=last-large-file-settings
    :start-func 'my=last-large-file-settings
    :documentation "Cycle between `my=shenanigans-on' and `my=shenanigans-off'")

  (defun my=racket-repl-clear ()
    (interactive)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (- (point-max) 2))))

  ;; (defun my=racket-repl-clear ()
  ;;   (interactive)
  ;;   (let ((inhibit-read-only t))
  ;;     (erase-buffer)))

  ;; See https://emacs.stackexchange.com/a/69010/36619
  (defmacro bind-chords (&rest args)
    "Bind multiple chords at once.

Accepts keyword argument:
:map - a keymap into which the keybindings should be added

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted)."
    (let* ((map (plist-get args :map))
           (maps (if (listp map) map (list map)))
           (key-bindings (progn
                           (while (keywordp (car args))
                             (pop args)
                             (pop args))
                           args)))
      (macroexp-progn
       (apply
        #'nconc
        (mapcar
         (lambda (form)
           (let ((command `(if (functionp ',(cdr form))
                               ',(cdr form)
                             #'(lambda () (interactive) ,(cdr form)))))
             (if maps
                 (mapcar
                  #'(lambda (m)
                      `(bind-chord ,(car form) ,command ,m)) maps)
               `((bind-chord ,(car form) ,command)))
             ;; (if maps
             ;;     (mapcar
             ;;      #'(lambda (m)
             ;;          `(bind-chord ,(car form) ',(cdr form) ,m)) maps)
             ;;   `((bind-chord ,(car form) ',(cdr form))))
             ))
         key-bindings)))))

;;;   (defmacro bind-chord (chord command &optional keymap)
;;;     "Bind CHORD to COMMAND in KEYMAP (`global-map' if not passed)."
;;;
;;;     (let* ((ommand `(if (functionp ,command)
;;;                              ,command
;;;                            #'(lambda () (interactive) ,command))))
;;;       (let ((key1 (logand 255 (aref chord 0)))
;;;             (key2 (logand 255 (aref chord 1))))
;;;         (if (eq key1 key2)
;;;             `(bind-key (vector 'key-chord ,key1 ,key2) ,ommand ,keymap)
;;;           `(progn
;;;              (bind-key (vector 'key-chord ,key1 ,key2) ,ommand ,keymap)
;;;              (bind-key (vector 'key-chord ,key2 ,key1) ,ommand ,keymap))))))
;;;
;;;   (bind-chords ("pq" . (my=insert-str "Yuuuuuuuuhuuu")))
;;;   (bind-chords ("pq"   . (my=insert-str "Yuuuuuuuuhuuu")))
;;;   (bind-keys ("C-c C-f"   . (my=insert-str "Yuuuuuuuuhuuu")))
;;;   (bind-keys :map global-map ("C-c C-f"   . (my=insert-str "Yuuuuhuuu")))

  (defun my=H-1 () (interactive) (message "H-1"))
  (defun my=H-2 () (interactive) (message "H-2"))
  (defun my=H-3 () (interactive) (message "H-3"))
  (defun my=H-4 () (interactive) (message "H-4"))

  (defun my=eval-bind-keys-and-chords ()
    "Revaluated by <s-+> replacement for e.g.:
  (global-set-key (kbd \"<s-f2>\") \\='eshell)
  (key-chord-define-global \"fj\" (lambda () (interactive)
                                             (my=insert-str \"()\" 1)))"
    (interactive)

    ;; (key-chord-define-global "fj" (lambda () (interactive)
    ;;                                 (my=insert-str "()" 1)))
    ;; (key-chord-define clojure-mode-map "fj" nil)
    ;; (key-chord-define global-map "fj" nil)

    ;; see also `key-chord-unset-global' / `key-chord-unset-local'
    ;; TODO this dolist block must be manually evaluated

    (dolist (state-map `(,clojure-mode-map ,cider-repl-mode-map))
      ;; (message "bind-chords %s" state-map) ;; TODO quote / unquote
      (bind-chords
       :map state-map
       ("pr" . (my=insert-str "(println \"\")" 2))
       ("rm" . (my=insert-str "(remove (fn []))" 3))
       ("fi" . my=clj-insert-filter-fn)
       ("de" . my=clj-insert-defn)
       ("db" . my=clj-insert-debugf)
       ("dg" . my=clj-insert-debugf)
       ("df" . my=clj-insert-fn)
       ("ds" . my=clj-insert-doseq)
       ("fn" . my=clj-insert-fn)
       ("do" . my=clj-insert-do)
       ("co" . my=clj-insert-comp)
       ("cd" . my=insert-clojuredocs)
       ("pa" . my=insert-partial)
       ("le" . my=clj-insert-let)
       ("fo" . my=clj-insert-for)
       ("ty" . my=clj-insert-type)
       ("ma" . my=clj-insert-map-fn)))

    (dolist (state-map `(,evil-ex-completion-map)) ;; not he evil-ex-map!!!
      (bind-keys :map state-map
                 ("s-0" . my=insert-group-parens)
                 ("s-)" . my=insert-group-parens))
      (bind-chords :map state-map
                   ("()" . my=insert-group-parens)))

    ;; (setq evil-respect-visual-line-mode t) doesn't work easily
    (global-set-key [remap move-beginning-of-line] 'crux-move-beginning-of-line)
    (global-set-key [remap evil-beginning-of-line] 'crux-move-beginning-of-line)

    (dolist (state-map `(,global-map))
      (bind-chords
       :map state-map
       ("KK" . my=switch-to-previous-buffer)
       ;; don't need to switch keyboards just because of parenthesis
       ("fj" . (my=insert-str "()" 1)))

      (bind-keys
       :map global-map
       ;; ("s-*"    . er/contract-region) ;; TODO see https://github.com/joshwnj

       ;; TODO The <escape> keybinding seems not to work.
       ;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
       ("<escape>"  . keyboard-escape-quit)
       ("s-K"       . my=kill-buffers--unwanted)
       ("s-C-K"     . my=kill-buffers--dired)
       ("s-R"       . spacemacs/rename-current-buffer-file)
       ("s-q"       . my=other-window)
       ("s-k"       . my=close-buffer)
       ("s-s"       . save-buffer)
       ("s-0"       . delete-window)
       ("s-1"       . my=delete-other-windows)
       ("<S-iso-lefttab>"   . next-buffer)
       ("<S-s-iso-lefttab>" . previous-buffer)
       ("<S-s-f8>"    . ace-swap-window)
       ;; ("<S-s-f8>"    . transpose-frame)
       ("s-N"       . spacemacs/cycle-defun-narrow-modes)
       ("s-n"       . spacemacs/cycle-narrow-widen)
       ;; ("s-2"    . my=split-other-window-below)
       ("s-2"       . split-window-below)   ; ~SPC w -~
       ;; see ~SPC w /~ and ~SPC w 2~
       ;; ("s-3"    . spacemacs/window-split-double-columns)
       ;; see ~SPC w /~ and ~SPC w 2~
       ("s-3"       . split-window-right-and-focus)
       ("s-9"       . my=load-layout)
       ("s-+"       . my=eval-bind-keys-and-chords)
       ("<s-kp-add>". my=eval-bind-keys-and-chords)
       ("s-z"       . my=buffer-selection-show)
       ;; dired: https://danlamanna .com/forget-scp-use-dired-dwim.html
       ("s-D"       . dired-jump)
       ("s-c"       . sp-copy-sexp)
       ("s-b"       . sp-backward-copy-sexp)
       ;; ("<f11>"     . bookmark-set)
       ;; ("<f11>"     . equake-toggle-fullscreen)
       ;; Move the parenthesis - see SPC k b/B/f/F
       ("<M-s-left>"  . sp-forward-barf-sexp)
       ("<M-s-right>" . sp-forward-slurp-sexp)
       ("<C-s-left>"  . sp-backward-slurp-sexp)
       ("<C-s-right>" . sp-backward-barf-sexp)
       ("s-;"         . spacemacs/comment-or-uncomment-lines)
       ("<S-s-f1>"    . eshell) ;; Shitf-Super-F1
       ("<s-f1>"      . projectile-multi-term-in-root)
       ("s-W"         . whitespace-cleanup)
       ("s-w"         . my=whitespace-mode-toggle)
       ("s-m"         . my=magit-status)
       ("<f3>"        . my=search-region-or-symbol)

       ("s-a"    . helm-mini)                   ; see advice-add my=helm-mini
       ("s-]"    . helm-mini)                   ; see advice-add my=helm-mini
       ("s-B"    . helm-filtered-bookmarks)
       ("<f9>"   . helm-filtered-bookmarks)
       ;; ("s-p" . helm-projectile)
       ("s-p"    . helm-projectile-find-file)
       ("M-s-p"  . helm-projectile-switch-project)
       ("<M-f3>" . spacemacs/helm-project-smart-do-search)
       ("s-f"    . helm-find-files)
       ("s-F"    . helm-recentf)       ; recentf-open-files
       ("s-r"    . helm-recentf)
       ("M-y"    . helm-show-kill-ring)    ; replaces evil-paste-pop
       ("s-G"    . helm-google-suggest)
       ("s-/"    . helm-swoop)
       ("s-l"    . lazy-helm/spacemacs/resume-last-search-buffer)

       ;; C-M-down default value is `down-list'
       ("<C-M-down>" . crux-duplicate-current-line-or-region)
       ("<C-s-down>" . crux-duplicate-current-line-or-region)
       ("C-c d"      . crux-duplicate-current-line-or-region)
       ("C-c t"      . crux-transpose-windows)
       ("<C-s-backspace>" . crux-kill-line-backwards) ; kill-line-backward
       ("s-j"             . crux-top-join-line)

       ("<C-up>"            . xah-backward-block)
       ("<C-down>"          . xah-forward-block)
       ;; TODO make pg-up / pg-down major-mode specific
       ;; ("<C-prior>"      . hs-hide-block)    ; pg-up
       ;; ("<C-next>"       . hs-show-block)    ; pg-down
       ;; ("<C-M-prior>"    . hs-toggle-hiding) ; pg-up
       ;; ("<C-M-prior>"    . hs-hide-all)      ; Ctrl + pg-up
       ;; ("<C-M-next>"     . hs-show-all)      ; Ctrl + pg-down
       ("<C-M-delete>"      . kill-sexp)
       ("<C-M-s-delete>"    . my=delete-next-sexp)
       ("<C-M-s-backspace>" . my=delete-prev-sexp)
       ("<C-M-backspace>"   . backward-kill-sexp)

       ("<s-backspace>"     . paredit-backward-kill-word)
       ("<s-delete>"        . paredit-forward-kill-word)
       ("s-M-SPC" . spacemacs/evil-search-clear-highlight)
       ("s-g"     . my=engine/search-or-browse)
       ("s-8"     . er/expand-region)   ; increase selected region by semantic units
       ("<f2>"    . evil-avy-goto-char-timer)
       ;; <S-tab> i.e. Shift-Tab i.e. <backtab> calls `next-buffer'
       ("<s-tab>" . spacemacs/alternate-buffer)
       ;; ("<s-tab>" . popwin:switch-to-last-buffer) ; - for popup buffers??
       ("<C-f2>"  . avy-goto-line) ;; binding clashes with xfce4-workspace
       ("C-s-/"   . avy-goto-line)

       ;; fd - evil-escape from insert state and everything else
       ;; occurences - function scope
       ("s-I"                . my=iedit-mode-toggle)
       ("s-i"                . iedit-mode)  ; all occurences in the buffer
       ;; ("s-i"             . spacemacs/enter-ahs-forward)
       ("<f12>"              . undo-tree-visualize)
       ;; ("<S-delete>"      . kill-region)
       ("<C-s-delete>"       . kill-line)   ; C-super-key
       ("<C-S-delete>"       . kill-line)   ; C-shift-key
       ;; ("s-l"                . spacemacs/resume-last-search-buffer)
       ("s-v" . my=evil-select-pasted)

       ;; TODO what's the difference between insert and insertchar?
       ("<S-s-insert>" . my=yank-and-select)

       ("s-L"        . spacemacs/cycle-line-number-types)
       ("C-s-l"      . spacemacs/cycle-large-file-settings)

       ;; jump like f/t in vim; TODO integrate zop-to-char with 'y' in evil
       ;; zop-up-to-char works as zop-to-char but stop just before target
       ("M-z"        . zop-up-to-char)
       ("M-Z"        . zop-to-char)

       ;; [1] spacemacs/move-text-transient-state/move-text-down
       ;; [2] spacemacs/move-text-transient-state/move-text-up
       ;; [1] and [2] don't drag:
       ("<M-down>"   . my=drag-stuff-down)
       ("<M-up>"     . my=drag-stuff-up)

       ("C-s-."      . spacemacs/jump-to-definition-other-window)
       ("s->"        . spacemacs/jump-to-definition-other-window)
       ("s-."        . spacemacs/jump-to-definition)

       ("s-,"        . evil-jump-backward)
       ;; ("s-,"     . dumb-jump-back)
       ;; ("s-,"     . cider-pop-back)

       ;; C-o; evil-jump-backward
       ;; C-i; evil-jump-forward; see dotspacemacs-distinguish-gui-tab

       ("<print>"    . describe-text-properties) ; my=what-face

       ("<s-return>"    . my=jump-last-edited-place)
       ("<C-s-return>"  . goto-last-change)
       ("s-J"        . evil-join)

       ("<s-print>"  . my=ediff-buffers-left-right) ; see advice-add
       ("s-A"        . align-regexp)
       ("s-:"        . my=fabricate-subst-cmd)

       ("s-<"         . my=select-in-ang-bracket)
       ("s-["         . my=select-in-sqr-bracket)
       ("s-("         . my=select-in-rnd-bracket)
       ("s-{"         . my=select-in-crl-bracket)
       ("s-\""        . my=select-in-string)

       ;; ("<C-mouse-5>" . (lambda () (interactive) (message "zoom-out")))
       ;; ("<C-mouse-4>" . (lambda () (interactive) (message "zoom-out")))

       ;; Set xfce4-keyboard-settings -> Layout -> Compose key: -
       ;; <menu> is not a prefix key. See:
       ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Keys.html
       ("H-1"      . my=H-1) ;; this doesn't work ("C-c h 1" . my=H-1)
       ("H-2"      . my=H-2)
       ("H-4"      . my=H-4) ;; this doesn't work ("C-c h 4" . my=H-4)
       ))
    (message "%s" "my=eval-bind-keys-and-chords evaluated")
    )

  ;; Thanx to https://github.com/fanhongtao/_emacs.d/blob/master/conf/my-key-modifiers.el
  ;; See also 'Have you Hyper for Great Good'
  ;; https://gist.github.com/toroidal-code/ec075dd05a23a8fb8af0
  ;; Also '<Multi_key> s s' should produce 'ß'
  (defun enable-hyper-super-modifiers-win32 ()
       ;;(setq w32-pass-apps-to-system nil)
       (setq w32-apps-modifier 'hyper)

       (setq w32-pass-lwindow-to-system nil)
       ;;(setq w32-phantom-key-code 42)  ;; what for?
       (setq w32-lwindow-modifier 'super)
       (setq w32-rwindow-modifier 'alt)
       )

  (defun enable-hyper-super-modifiers-linux-x ()
    (interactive)
    ;; on nowadays linux, <windows> key is usually configured to Super

    ;; menu key as hyper (for H-s release <menu> key before pressing 's')
    (define-key key-translation-map [menu] 'event-apply-hyper-modifier) ;H-
    ;; (define-key key-translation-map [apps] 'event-apply-hyper-modifier)

    ;; by default, Emacs bind <menu> to execute-extended-command (same as M-x)
    ;; now <menu> defined as 'hyper, we need to press <menu> twice to get <H-menu>
    ;; (global-set-key (kbd "<H-menu>") 'execute-extended-command)
    ;; (global-unset-key (kbd "<menu>"))
    ;; (global-unset-key (kbd "<H-menu>"))
    )

  (defun enable-hyper-super-modifiers-linux-console ()
    (message "fixme: enable-hyper-super-modifiers-linux-console"))

  (defun enable-hyper-super-modifiers-macos ()
    ;; http://xahlee.org/emacs/emacs_hyper_super_keys.html
    (setq mac-option-modifier 'hyper) ; sets the Option key as Hyper
    (setq mac-option-modifier 'super) ; sets the Option key as Super
    (setq mac-command-modifier 'meta) ; sets the Command key as Meta
    (setq mac-control-modifier 'meta) ; sets the Control key as Meta
    )

  (defun enable-hyper-super-modifiers ()
    (let ( (frame (framep (selected-frame))) )
      (cond
       ((memq frame '(w32 win32))
        (enable-hyper-super-modifiers-win32) )
       ((eq frame 'x)
        (enable-hyper-super-modifiers-linux-x ) )
       ((eq frame 'ns)
        (enable-hyper-super-modifiers-macos) )
       (frame
        (enable-hyper-super-modifiers-linux-console ))
       (t
        (message "fixmed: enable-hyper-super-modifiers") )
       ))

    ;; you can always use "C-c h" as 'hyper modifier, even in Linux console or DOS
    (define-key key-translation-map (kbd "C-c h") 'event-apply-hyper-modifier)
    (define-key key-translation-map (kbd "C-c s") 'event-apply-super-modifier)
    (define-key key-translation-map (kbd "C-c a") 'event-apply-alt-modifier)
    )
  (enable-hyper-super-modifiers)
  ;; this doesn't work:
  (define-key key-translation-map (kbd "H-3") (kbd "•")) ; bullet

  (my=eval-bind-keys-and-chords) ; <s-kp-add>

  ;; BUG: "<s-kp-insert>" "<C-insert>" are the same keys Uhg?
  ;; ("<s-kp-insert>" .)
  ;; ("<s-kp-0>"      .)
  ;; ("s-'"           .)
  ;; (unbind-key "<C-insert>" &optional keymap)
  ;; ("<C-insert>"    .)

  (with-eval-after-load 'magit-mode
    (bind-keys
     :map magit-mode-map
     ;; Workaround for the
     ;; https://github.com/emacs-evil/evil-collection/issues/554
     ("C-v" . evil-visual-line)
     ("1"   . magit-section-show-level-1-all)
     ("2"   . magit-section-show-level-2-all)
     ("3"   . magit-section-show-level-3-all)
     ("4"   . magit-section-show-level-4-all)
     ;; overshadows `(digit-argument <n>)'; use C-M-<n> instead
     ("C-1" . magit-section-show-level-1)
     ("C-2" . magit-section-show-level-2)
     ("C-3" . magit-section-show-level-3)
     ("C-4" . magit-section-show-level-4)))

  (bind-keys
   :map term-raw-map
   ("C-<right>" . right-word)
   ("C-<left>" . left-word)
   ("<prior>" . evil-scroll-page-up)
   ("<next>"  . evil-scroll-page-down))

  (bind-keys
   :map dired-mode-map
   ("<f5>"        . revert-buffer)
   ("C-s-h"       . my=dired-dotfiles-toggle) ;; "C-H" doesn't work WTF???
   ("<backspace>" . (lambda () (interactive) (find-alternate-file "..")))
   ;; See https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer
   ;; ("<return>"    . dired-find-alternate-file)
   ;; ("<return>"    . dired-x-find-file) ;; asks for file instead of opening it
   ;; ("<return>"    . diredp-find-file-reuse-dir-buffer)
   ("<return>"    . dired-find-file) ;; default
   ("<S-delete>"  . my=dired-do-delete))

  ;; (eval-after-load "dired"
  ;;   '(progn
  ;;      (defadvice dired-advertised-find-file (around dired-subst-directory
  ;;                                                    activate)
  ;;        "Replace current buffer if file is a directory."
  ;;        (interactive)
  ;;        (message "%s" 'dired-advertised-find-file)
  ;;        (let* ((orig (current-buffer))
  ;;               ;; (filename (dired-get-filename))
  ;;               (filename (dired-get-filename t t))
  ;;               (bye-p (file-directory-p filename)))
  ;;          ad-do-it
  ;;          (when (and bye-p (not (string-match "[/\\\\]\\.$" filename)))
  ;;            (kill-buffer orig))))))

  ;; (eval-after-load "dired"
  ;;   ;; don't remove `other-window', the caller expects it to be there
  ;;   '(defun dired-up-directory (&optional other-window)
  ;;      "Run Dired on parent directory of current directory."
  ;;      (interactive "P")
  ;;      (let* ((dir (dired-current-directory))
  ;;             (orig (current-buffer))
  ;;             (up (file-name-directory (directory-file-name dir))))
  ;;        (or (dired-goto-file (directory-file-name dir))
  ;;            ;; Only try dired-goto-subdir if buffer has more than one dir.
  ;;            (and (cdr dired-subdir-alist)
  ;;                 (dired-goto-subdir up))
  ;;            (progn
  ;;              (kill-buffer orig)
  ;;              (dired up)
  ;;              (dired-goto-file dir))))))

  (bind-keys :map paredit-mode-map
             ;; these keybindings don't work in the cider-repl-mode-map
             ("<C-right>"    . right-word)
             ("<C-left>"     . left-word))

  (dolist (state-map `(,clojure-mode-map ,cider-repl-mode-map))
    (bind-keys
     :map state-map
     ;; on the german keyboard the '#' is next to Enter
     ("C-s-\\" . my=clj-toggle-reader-comment-current-sexp)
     ("s-\\"   . my=clj-toggle-reader-comment-fst-sexp-on-line)
     ("s-X"   . my=switch-to-repl-start-figwheel)
     ("s-e"   . cider-eval-last-sexp)
     ("s-j"   . cider-format-defun)
     ("s-i"   . cljr-rename-symbol)))

  (bind-keys
   :map cider-repl-mode-map
   ("<menu>" . my=stop-synths-metronoms)
   ("s-h"    . helm-cider-history)
   ("s-j"    . cider-format-defun)
   ("s-x"    . cider-switch-to-last-clojure-buffer)
   ("M-s-l"  . my=cider-reload-ns-from-file)
   ("s-u"    . my=cider-reload-ns-from-file)
   ;; invoke from clojure buffer
   ("<C-s-delete>" . cider-repl-clear-buffer))

  (bind-keys
   :map clojure-mode-map
   ("s-d"    . cider-eval-defun-at-point)
   ("s-x"    . my=cider-switch-to-repl-buffer)
   ("C-s-c"  . cider-connect-clj)
   ("C-s-j"  . cider-jack-in)
   ;; ("s-r" . cider-eval-last-expression-in-repl)
   ("M-s-l"  . my=cider-save-and-load-current-buffer)
   ("s-u"    . my=cider-save-and-load-current-buffer)
   ("M-s-n"  . cider-repl-set-ns)
   ("s-t"    . cider-test-run-tests)

   ;; TODO see global-map keybindings
   ;; ("s-."  . cider-find-var)
   ;; ("s-,"  . cider-pop-back)
   ;; TODO s-M does not work in REPL buffer

   ;; Reload modified and unloaded namespaces on the classpath
   ("s-o"     . cider-ns-refresh)

   ;; Send a (require ’ns :reload) to the REPL
   ;; ("s-o"  . cider-ns-reload)

   ("C-s-o"   . my=cider-clear-compilation-highlights))

  (bind-chords
   :map emacs-lisp-mode-map
   ("ms" . my=elisp-insert-message)
   ("df" . my=elisp-insert-defun))

  ;; The read syntax #'. See
  ;; https://endlessparentheses.com/get-in-the-habit-of-using-sharp-quote.html
  (defun endless/sharp ()
    "Insert #' unless in a string or comment."
    (interactive)
    (call-interactively #'self-insert-command)
    (let ((ppss (syntax-ppss)))
      (unless (or (elt ppss 3)
                  (elt ppss 4)
                  (eq (char-after) ?'))
        (insert "'"))))

  (bind-keys
   :map emacs-lisp-mode-map
   ("C-s-l" . my=elisp-insert-let)
   ("C-s-m" . my=elisp-insert-message)
   ("C-s-p" . my=elisp-insert-message)
   ("C-s-d" . my=elisp-insert-defun)
   ("s-d"   . my=eval-current-defun)
   ("#"     . endless/sharp)
   ;; emacs-lisp doesn't have a syntax for sexp-comment
   ("s-\\"   . spacemacs/comment-or-uncomment-lines)
   )

  (dolist (state-map `(,lisp-mode-shared-map ; lisp-mode-map doesn't work
                       ,clojure-mode-map))
    (bind-keys
     :map state-map
     ("<C-M-right>" . end-of-defun)       ; default is forward-sexp
     ("<C-M-left>"  . beginning-of-defun) ; default is backward-sexp
     ))

  (bind-keys
   :map org-mode-map
   ("<menu>"      . org-latex-export-to-pdf))

  (bind-keys
   :map prog-mode-map
   ;; M-/  M-x hippie-expand
   ("s-Q" . dumb-jump-quick-look)
   ("s-h" . spacemacs/helm-jump-in-buffer)
   ;; previously: helm-imenu-in-all-buffers
   ("s-H" . lazy-helm/helm-imenu-in-all-buffers)
   ("s-u" . eval-buffer)
   ("s-e" . eval-last-sexp))

  (add-hook
   'LaTeX-mode-hook
   (lambda ()
     (bind-keys :map LaTeX-mode-map ("<menu>" . latex/build))))

  (add-hook
   'python-mode-hook
   (lambda ()
     (bind-keys :map python-mode-map
                ("s-x" . spacemacs/python-start-or-switch-repl))))
  (add-hook
   'debugger-mode-hook
   (lambda ()
     (bind-keys :map debugger-mode-map ("C-g" . debugger-quit))))

  (defun my=bind-keys-racket (hook state-map)
    (add-hook
     hook
     (lambda ()
       (bind-keys
        :map state-map
        ("s-o" . racket-run-and-switch-to-repl)
        ("<C-s-delete>" . my=racket-repl-clear)
        ("M-s-d"  . my=racket-insert-fn)
        ("M-s-p"  . my=insert-partial)
        ("C-s-p"  . my=racket-insert-log)
        ("C-s-\\" . my=racket-toggle-reader-comment-current-sexp)
        ("s-\\"   . my=racket-toggle-reader-comment-fst-sexp-on-line)))))

  (my=bind-keys-racket 'racket-mode-hook      'racket-mode-map)
  (my=bind-keys-racket 'racket-repl-mode-hook 'racket-repl-mode-map)

  (defun my=bind-keys-scheme (hook state-map)
    (add-hook
     hook
     (lambda ()
       (bind-keys
        :map state-map
        ;; ("s-;"    . my=racket-toggle-reader-comment-current-sexp)
        ("s-x"    . geiser-mode-switch-to-repl)
        ("C-s-\\" . my=racket-toggle-reader-comment-current-sexp)
        ("s-\\"   . my=racket-toggle-reader-comment-fst-sexp-on-line)))))

  (my=bind-keys-racket 'scheme-mode-hook      'scheme-mode-map)
  (my=bind-keys-racket 'scheme-repl-mode-hook 'scheme-repl-mode-map)

  (my=bind-keys-racket 'geiser-mode-hook      'geiser-mode-map)
  (my=bind-keys-racket 'geiser-repl-mode-hook 'geiser-repl-mode-map)
  ;; (bind-keys :map helm-mode-map)

  ;; advice, defadvice and letf shouldn't be used:
  ;; https://lists.gnu.org/archive/html/emacs-devel/2012-12/msg00146.html
  ;; Emacs 24.4 replaces this mechanism with advice-add

  ;; Difference between `evil-search-forward` and `evil-ex-search-forward`:
  ;; evil-search-forward    - wrap emacs isearch-forward
  ;; evil-ex-search-forward - invoke the evil internal search
  ;; https://emacs.stackexchange.com/a/24913

  ;; See
  ;; https://www.reddit.com/r/emacs/comments/6ewd0h/how_can_i_center_the_search_results_vertically/?utm_source=share&utm_medium=web2x

  (advice-add
   #'split-window-right-and-focus
   :after (defun my=recenter-top-bottom ()
            ;; needed cause the (recenter-top-bottom) has (interactive "P")
            (recenter-top-bottom)))
  (advice-add
   #'whitespace-cleanup
   :after (defun my=whitespace-cleanup ()
            (message "whitespace-cleanup")))
  (advice-add
   #'evil-avy-goto-char-timer
   :after (defun my=evil-avy-goto-char-timer ()
            (message "evil-avy-goto-char-timer: SPC j j, <f2>")))
  (advice-add
   #'avy-goto-line
   :after (defun my=avy-goto-line ()
            (message "avy-goto-line: SPC j l, M-m j l, <C-f2>, C-s-/")))
  (advice-add #'evil-ex-search-next      :after #'evil-scroll-line-to-center)
  (advice-add #'evil-ex-search-previous  :after #'evil-scroll-line-to-center)

  (advice-add #'ediff-quit :around #'my=disable-y-or-n-p)
  (advice-add #'helm-mini  :before #'my=helm-mini)

  ;; TODO workaround for (global-set-key (kbd "C-M-k") 'kill-sexp) overridden by
  ;; layers/+misc/multiple-cursors/packages.el
  (dolist (state-map `(,evil-normal-state-map ,evil-insert-state-map))
    (bind-keys :map state-map
               ("C-M-k" . kill-sexp)))

  (dolist (state-map `(,evil-motion-state-map ,evil-visual-state-map))
    ;; Move by screen lines instead of logical (long) lines
    (bind-keys
     :map state-map
     ("j" . evil-next-visual-line)
     ("k" . evil-previous-visual-line)))

  (bind-keys
   :map evil-visual-state-map
   ("p" . my=evil-paste-after-from-0))

  ;; see also binding for <f2>
  ;; (bind-keys :map evil-normal-state-map
  ;;            ("f" . evil-avy-goto-char-timer)
  ;;            ("t" . evil-avy-goto-char-timer))

  ;; (add-to-list 'spacemacs-indent-sensitive-modes 'clojure-mode)
  ;; (add-to-list 'spacemacs-indent-sensitive-modes 'clojurescript-mode)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
