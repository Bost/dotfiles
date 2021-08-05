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
   '("~/dev/dotfiles/emacs/")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; TODO try out ibuffer
     ;; (ibuffer :variables ibuffer-group-buffers-by 'projects)

     (erc :variables
          ;; erc-enable-notifications nil
          erc-server-list
          '(("irc.libera.chat" :port "6667" :ssl t :nick "bost" :password "")))

     php
     ;; typescript
     (typescript :variables
                 javascript-backend 'tide
                 ;; javascript-backend 'lsp
                 typescript-fmt-tool 'prettier
                 typescript-linter 'eslint
                 ;; typescript-fmt-on-save t
                 )
     (javascript :variables
                 javascript-backend 'tide
                 javascript-fmt-tool 'prettier
                 node-add-modules-path t)

     asciidoc
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; auto-completion
     (auto-completion :variables
                      ;; (setq
                      auto-completion-enable-help-tooltip t
                      ;; auto-completion-enable-help-tooltip 'manual
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      ;; )
                      )
     better-defaults
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
     helm
     multiple-cursors

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

          ;; :bind (
          ;;   ;; Show list of references to a given node from other nodes
          ;;   ("C-c n l" . org-roam-buffer-toggle)
          ;;   ("C-c n f" . org-roam-node-find)
          ;;   ("C-c n i" . org-roam-node-insert))

          ;; org-roam-directory "~/org-roam" default

          org-enable-roam-support t
          ;; A Web Application to Visualize the Org-Roam Database
          ;; org-enable-roam-server t

          ;; capture content from external applications such as the browser
          ;; org-enable-roam-protocol t
          )
     rust
     scheme ;; requires guile-2.2; M-x run-guile
     shell-scripts
     haskell
     csv
     (python
      :variables
      ;; TODO use a list of prefered python interpreters
      python-shell-interpreter "python3.8"; "python3.7" ; "python3.6"
      ;; -i     : inspect interactively after running script; forces a prompt even
      ;; if stdin does not appear to be a terminal; also PYTHONINSPECT=x
      python-shell-interpreter-args "-i")

     ;; Show commands as you type in a separate buffer
     ;; command-log  ;; deprecated

     (colors :variables
             colors-enable-nyan-cat-progress-bar t)

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
     systemd
     hy ;; hylang - lisp embedded in python
     go
     yaml
     docker

     ;; Racket settings for emacs
     ;; https://gist.github.com/soegaard/942a3074513655292816e0b79c466620
     racket ;; see https://racket-mode.com

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

     ;; JSX major mode. JSX is an XML-like syntax extension to ECMAScript
     rjsx-mode

     ;; Minor mode to format JS code on file save
     prettier-js

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
     helm-cider-history
     cider-hydra ;; pop-up menus of commands with common prefixes for CIDER

     ;; Emacs mode for the Lean theorem prover.
     ;; lean-mode
     ;; helm-lean

     evil-vimish-fold

     ;; crosshairs-mode messes up with the background color of the current-line
     ;; (crosshairs    :location local)
     ;; (hl-line+      :location local)
     ;; (vline         :location local)
     ;; (col-highlight :location local)

     cobol-mode

     ;; org-mode-babel packages {{{
     ;; see also org-babel-load-languages
     (ob-racket     :location (recipe :fetcher github :repo "hasu/emacs-ob-racket"))
     ;; (scribble-mode :location (recipe :fetcher github :repo "emacs-pe/scribble-mode"))
     ;; }}}

     helm-system-packages
     ;; helm-descbinds
     ;; helm-slime
     helm-dictionary

     ;; telegram client for emacs
     telega

     ;; dired-x is dired extended by:
     ;;     Omitting uninteresting files
     ;;     Guessing shell commands
     ;;     Running Dired command in non-Dired
     ;;     Finding a file mentioned in a buffer
     ;;     Commands using file marking
     dired-x
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
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
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
   dotspacemacs-themes '(spacemacs-light
                         spacemacs-dark
                         twilight-anti-bright
                         underwater
                         solarized-dark-high-contrast)

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
   dotspacemacs-auto-resume-layouts t

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
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
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

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non nil. (default nil)
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
   ;; and todos. If non nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first.")


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump.")


(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; typescript-indent-level is overridden by project-specific .editorconfig
  ;; (setq-default typescript-indent-level 4)

  ;; (sp-use-paredit-bindings)

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
  (global-company-mode)

  (add-to-list 'auto-mode-alist '("\\.cob" . cobol-mode))

  ;; (push '(clojuredocs
  ;;         :name "Clojure Docs"
  ;;         :url "http://clojuredocs.org/clojure.core/%s")
  ;;       search-engine-alist)
  (beacon-mode 1)
  (blink-cursor-mode t)
  ;; (spacemacs/toggle-menu-bar-on)
  ;; (global-prettify-symbols-mode +1)

  (my=spacemacs-light--highlight-current-line)

  (setq

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
   bookmark-default-file "~/dev/dotfiles/emacs/bookmarks"
   ;; Hotfix of "magit ediff on unstaged file leads to emacs freeze. #4730"
   ediff-window-setup-function 'ediff-setup-windows-default

   ;; Fix projectile-regenerate-tags: ctags: invalid option -- ’e’
   ;; See https://github.com/bbatsov/projectile/issues/133
   projectile-tags-command "ctags-exuberant -Re -f \"%s\" %s"

   create-lockfiles nil ;; do not create .# lockfiles
   vc-follow-symlinks t ;; auto follow symbolic links
   browse-url-browser-function 'browse-url-default-browser
   ;; '(("wikipedia\\.org" . browse-url-firefox)
   ;;   ("github" . browse-url-chromium)
   ;;   ("thefreedictionary\\.com" . eww-browse-url)
   ;;   ("." . browse-url-default-browser))

   font-latex-fontify-script nil
   org-latex-listings 'minted
   org-latex-packages-alist '(("" "minted"))
   org-latex-pdf-process
   '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")

   key-chord-two-keys-delay 0.02 ;; default is 0.1
   ;; color-identifiers-mode t
   )

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
     ))

  (defalias 'save-selected-text 'write-region)

  ;; Alternativelly in the package auto-dim-other-buffers
  ;; define and use some of the faces:
  ;;     font-lock-builtin-face
  ;;     font-lock-comment-delimiter-face
  ;;     font-lock-comment-face
  ;;     font-lock-constant-face
  ;;     font-lock-function-name-face
  ;;     font-lock-negation-char-face
  ;;     font-lock-preprocessor-face
  ;;     font-lock-regexp-grouping-construct
  ;;     font-lock-regexp-grouping-backslash
  ;;     font-lock-string-face
  ;;     font-lock-type-face
  ;;     font-lock-variable-name-face
  ;;     font-lock-warning-face
  ;;     font-lock-doc-face
  ;;     font-lock-keyword-face
  ;;     font-lock-comment-face
  ;; analog to auto-dim-other-buffers-face
  (make-face 'flash-active-buffer-face)

  (set-face-attribute 'flash-active-buffer-face nil
                      :background "black"
                      :foreground nil)

  (key-chord-mode 1)

  ;; (progn
  ;;   (unbind-key "<f5>" cider-repl-mode-map)
  ;;   (unbind-key "<f6>" cider-repl-mode-map)
  ;;   (unbind-key "<f7>" cider-repl-mode-map)
  ;;   (unbind-key "<f5>" clojure-mode-map)
  ;;   (unbind-key "<f6>" clojure-mode-map)
  ;;   (unbind-key "<f7>" clojure-mode-map))

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
  (use-package cider)        ;; must be here for the bind-keys

  ;; TODO my=eval-bind-keys-and-chords
  ;; ~SPC m e c~ or M-x spacemacs/eval-current-form-sp

  ;; TODO autoload
  (spacemacs/declare-prefix "og" "google-this")
  (spacemacs/set-leader-keys
    "ogg" 'google-this
    "ogr" 'google-this-region
    "oc"  'my=cider-clear-compilation-highlights
    ;; "of"  'my=switch-to-repl-start-figwheel
    "or"  'rotate-frame
    ;; Show list of references to a given node from other nodes
    "ob"  'org-roam-buffer-toggle
    "of"  'org-roam-node-find
    "oi"  'org-roam-node-insert
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

  (defun my=eval-bind-keys-and-chords ()
    "Revaluated by <s-+> replacement for e.g.:
  (global-set-key (kbd \"<s-f2>\") \\='eshell)
  (key-chord-define-global \"fj\" (lambda () (interactive) (my=insert-str \"()\" 1)))"
    (interactive)

    ;; (defun my=chord (chord text &optional n-chars-back)
    ;;   "(key-chord-unset-global \"fj\")"
    ;;   (key-chord-define-global chord
    ;;                            (lambda ()
    ;;                              (interactive)
    ;;                              (if text
    ;;                                  (progn
    ;;                                    (insert text)
    ;;                                    (if n-chars-back
    ;;                                        (left-char n-chars-back)))
    ;;                                (message "text is undefined" text)))))
    ;; (my=chord "fj" "fox" 1)

    ;; (key-chord-define-global "fj" (lambda () (interactive) (my=insert-str "()" 1)))
    ;; (key-chord-define clojure-mode-map "fj" nil)
    ;; (key-chord-define global-map "fj" nil)

    ;; see also `key-chord-unset-global' / `key-chord-unset-local'
    ;; TODO this dolist block must be manually evaluated
    (dolist (state-map `(,clojure-mode-map ,cider-repl-mode-map))
      ;; (message "bind-chords %s" state-map) ;; TODO quote / unquote
      (bind-chords :map state-map
                   ("pr" . (lambda () (interactive) (my=insert-str "(println \"\" )" 3)))
                   ("rm" . my=clj-insert-remove-fn)
                   ("fi" . my=clj-insert-filter-fn)
                   ("de" . my=clj-insert-defn)
                   ("df" . my=clj-insert-fn)
                   ("fn" . my=clj-insert-fn)
                   ("do" . my=clj-insert-do)
                   ("co" . my=clj-insert-comp)
                   ("cd" . my=insert-clojuredocs)
                   ("pa" . my=insert-partial)
                   ("le" . my=clj-insert-let)
                   ("fo" . my=clj-insert-for)
                   ("ty" . my=clj-insert-type)
                   ("ma" . my=clj-insert-map-fn)))
    ;; Max time delay between two key presses to be considered a key chord
    ;; (setq key-chord-two-keys-delay 0.1) ; default 0.1
    ;; Max time delay between two presses of the same key to be considered a key chord.
    ;; Should normally be a little longer than `key-chord-two-keys-delay'.
    ;; (setq key-chord-one-key-delay 0.2) ; default 0.2
    (dolist (state-map `(,global-map))
      (bind-chords :map state-map
                   ("KK" . my=switch-to-previous-buffer)
                   ;; don't need to switch keyboards just because of parenthesis
                   ("fj" . (lambda () (interactive) (my=insert-str "()" 1)))))

    ;; (setq evil-respect-visual-line-mode t) doesn't work easily
    (global-set-key [remap move-beginning-of-line] 'crux-move-beginning-of-line)
    (global-set-key [remap evil-beginning-of-line] 'crux-move-beginning-of-line)

    (bind-keys
     :map global-map
     ;; ("s-*"    . er/contract-region) ;; TODO see https://github.com/joshwnj
     ("s-K"       . my=kill-buffers--unwanted)
     ("s-C-K"     . my=kill-buffers--dired)
     ("s-R"       . spacemacs/rename-current-buffer-file)
     ("s-q"       . other-window) ; straight jump to window: SPC 0, SPC 1 ...
     ("s-k"       . my=close-buffer)
     ("s-s"       . save-buffer)
     ("s-0"       . delete-window)
     ("s-1"       . my=delete-other-windows)
     ("<S-iso-lefttab>"   . next-buffer)
     ("<S-s-iso-lefttab>" . previous-buffer)
     ("<s-f8>"    . ace-swap-window)
     ;; ("<s-f8>"    . transpose-frame)
     ("s-N"       . spacemacs/cycle-defun-narrow-modes)
     ("s-n"       . spacemacs/cycle-narrow-widen)
     ;; ("s-2"    . my=split-other-window-below)
     ;; ("s-3"    . my=split-other-window-right)
     ("s-2"       . split-window-below)   ; SPC w -
     ;; ("s-3"    . spacemacs/window-split-double-columns) ; SPC w 2
     ("s-3"       . split-window-right-and-focus) ; SPC w 3
     ("s-9"       . my=load-layout)
     ("s-+"       . my=eval-bind-keys-and-chords)
     ("<s-kp-add>". my=eval-bind-keys-and-chords)
     ("s-z"       . my=buffer-selection-show)
     ;; dired: https://danlamanna .com/forget-scp-use-dired-dwim.html
     ("s-D"       . dired-jump)
     ("s-c"       . sp-copy-sexp)
     ("s-b"       . sp-backward-copy-sexp)
     ("s-B"       . helm-filtered-bookmarks)
     ("<f9>"      . helm-filtered-bookmarks)
     ("<f11>"     . bookmark-set)
     ;; Move the parenthesis - see SPC k b/B/f/F
     ("<M-s-left>"  . sp-forward-barf-sexp)
     ("<M-s-right>" . sp-forward-slurp-sexp)
     ("<C-s-left>"  . sp-backward-slurp-sexp)
     ("<C-s-right>" . sp-backward-barf-sexp)
     ("s-;"         . spacemacs/comment-or-uncomment-lines)
     ("<S-s-f1>"    . eshell) ;; Shitf-Super-F1
     ("<s-f1>"      . projectile-multi-term-in-root)
     ;; ("s-p"      . helm-projectile)
     ("s-p"         . helm-projectile-find-file)
     ("M-s-p"       . helm-projectile-switch-project)
     ("s-W"         . my=whitespace-cleanup)
     ("s-w"         . my=whitespace-mode-toggle)
     ("s-m"         . my=magit-status)
     ("<f3>"        . my=search-region-or-symbol)
     ("<M-f3>"      . spacemacs/helm-project-smart-do-search)
     ("s-f"         . helm-find-files)
     ("s-F"         . helm-recentf)       ; recentf-open-files
     ("s-r"         . helm-recentf)

     ("<C-M-down>" . crux-duplicate-current-line-or-region) ; default is down-list
     ("<C-s-down>" . crux-duplicate-current-line-or-region)
     ("C-c d"      . crux-duplicate-current-line-or-region)
     ("C-c t"      . crux-transpose-windows)
     ("<C-s-backspace>" . crux-kill-line-backwards) ; kill-line-backward
     ("s-j"             . crux-top-join-line)

     ("<C-up>"            . xah-backward-block)
     ("<C-down>"          . xah-forward-block)
     ("<C-prior>"         . hs-hide-block) ; pg-up
     ("<C-next>"          . hs-show-block) ; pg-down
     ;; ("<C-M-prior>"    . hs-toggle-hiding)
     ("<C-M-prior>"       . hs-hide-all)  ; pg-up
     ("<C-M-next>"        . hs-show-all)  ; pg-down
     ("<C-M-delete>"      . kill-sexp)
     ("<C-M-s-delete>"    . my=delete-next-sexp)
     ("<C-M-s-backspace>" . my=delete-prev-sexp)
     ("<C-M-backspace>"   . backward-kill-sexp)

     ("<s-backspace>"     . paredit-backward-kill-word)
     ("<s-delete>"        . paredit-forward-kill-word)
     ("s-M-SPC" . spacemacs/evil-search-clear-highlight)
     ("M-y"     . helm-show-kill-ring)    ; replaces evil-paste-pop
     ("s-g"     . my=engine/search-or-browse)
     ("s-G"     . helm-google-suggest)
     ("s-8"     . er/expand-region)   ; increase selected region by semantic units
     ("<f2>"    . my=evil-avy-goto-char-timer)
     ("s-/"     . helm-swoop)
     ("<s-tab>" . my=alternate-buffer)  ;; Shift-Tab <backtab>
     ("<C-f2>"  . my=avy-goto-line)
     ("C-s-/"   . my=avy-goto-line)

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
     ("s-l"                . lazy-helm/spacemacs/resume-last-search-buffer)
     ;; `s-SPC v' but it overrides the `expand region' menu point
     ;; (evil-leader/set-key "v" 'my=evil-select-pasted)

     ("s-L"                . spacemacs/cycle-line-number-types)
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
     ("s-a"        . helm-mini)                   ; see advice-add my=helm-mini
     ("s-]"        . helm-mini)                   ; see advice-add my=helm-mini
     ("s-A"        . align-regexp)
     ("s-:"        . my=fabricate-subst-cmd)

     ("s-<"         . my=select-in-ang-bracket)
     ("s-["         . my=select-in-sqr-bracket)
     ("s-("         . my=select-in-rnd-bracket)
     ("s-{"         . my=select-in-crl-bracket)
     ("s-\""        . my=select-in-string)

     ;; ("<C-mouse-5>" . (lambda () (interactive) (message "zoom-out")))
     ;; ("<C-mouse-4>" . (lambda () (interactive) (message "zoom-out")))
     ;; <menu> is not a prefix key. See:
     ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Keys.html
     ;; ("<menu>"      . (lambda () (interactive) (message "context-menu")))
     )
    (message "%s" "my=eval-bind-keys-and-chords evaluated")
    )

  (my=eval-bind-keys-and-chords) ; <s-kp-add>

  ;; BUG: "<s-kp-insert>" "<C-insert>" are the same keys Uhg?
  ;; ("<s-kp-insert>" .)
  ;; ("<s-kp-0>"      .)
  ;; ("s-'"           .)
  ;; (unbind-key "<C-insert>" &optional keymap)
  ;; ("<C-insert>"    .)

  (bind-keys :map magit-mode-map
             ("1"   . magit-section-show-level-1-all)
             ("2"   . magit-section-show-level-2-all)
             ("3"   . magit-section-show-level-3-all)
             ("4"   . magit-section-show-level-4-all)
             ;; overshadows `(digit-argument <n>)'; use C-M-<n> instead
             ("C-1" . magit-section-show-level-1)
             ("C-2" . magit-section-show-level-2)
             ("C-3" . magit-section-show-level-3)
             ("C-4" . magit-section-show-level-4))

  (bind-keys :map dired-mode-map
             ("<f5>" . revert-buffer)
             ("C-h" . my=dired-dotfiles-toggle)
             ("<backspace>" . dired-up-directory)
             ("<S-delete>"  . dired-do-delete))

  (bind-keys :map paredit-mode-map
             ;; these keybindings don't work in the cider-repl-mode-map
             ("<C-right>"    . right-word)
             ("<C-left>"     . left-word))

  (dolist (state-map `(,clojure-mode-map ,cider-repl-mode-map))
    (bind-keys :map state-map
               ;; on the german keyboard the '#' is next to Enter
               ("C-s-\\" . my=clj-toggle-reader-comment-current-sexp)
               ("s-\\"   . my=clj-toggle-reader-comment-fst-sexp-on-line)

               ("<f5>"  . my=telegram-restart)
               ("<f6>"  . my=web-restart)
               ("<f7>"  . my=show-pic)
               ("<f8>"  . my=show-pic-for-pred)

               ("s-X"   . my=switch-to-repl-start-figwheel)
               ("s-e"   . cider-eval-last-sexp)
               ("s-j"   . cider-format-defun)
               ("s-i"   . cljr-rename-symbol)

               ("C-s-o" . my=clj-insert-do)
               ("C-s-f" . my=clj-insert-filter-fn)
               ("C-s-r" . my=clj-insert-remove-fn)
               ("C-s-l" . my=clj-insert-let)
               ("C-s-m" . my=clj-insert-map-fn)

               ("C-s-d" . my=clj-insert-defn)

               ("M-s-e" . my=clj-insert-def)
               ("C-s-e" . my=clj-insert-def)

               ("M-s-d" . my=clj-insert-fn)

               ("M-s-c" . my=clj-insert-comp)
               ("C-s-c" . my=clj-insert-comp)

               ("M-s-p" . my=insert-partial)
               ("C-s-p" . my=clj-insert-log)

               ("C-s-s" . my=clj-insert-doseq)
               ("C-s-t" . my=clj-insert-type)))

  (bind-keys :map cider-repl-mode-map
             ("<menu>"       . my=stop-synths-metronoms)
             ("s-h"          . helm-cider-history)
             ("s-j"          . cider-format-defun)
             ("s-x"          . cider-switch-to-last-clojure-buffer)
             ("M-s-l"  . my=cider-reload-ns-from-file)
             ("s-u"    . my=cider-reload-ns-from-file)
             ;; invoke from clojure buffer
             ("<C-s-delete>" . cider-repl-clear-buffer))

  (bind-keys :map clojure-mode-map
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

  (bind-chords :map emacs-lisp-mode-map
               ("ms" . my=elisp-insert-message)
               ("df" . my=elisp-insert-defun))

  (bind-keys :map emacs-lisp-mode-map
             ("C-s-l" . my=elisp-insert-let)
             ("C-s-m" . my=elisp-insert-message)
             ("C-s-p" . my=elisp-insert-message)
             ("C-s-d" . my=elisp-insert-defun)
             ("s-d"   . my=eval-current-defun)
             )

  (dolist (state-map `(,lisp-mode-shared-map ; lisp-mode-map doesn't work
                       ,clojure-mode-map))
    (bind-keys :map state-map
               ("<C-M-right>" . end-of-defun)       ; default is forward-sexp
               ("<C-M-left>"  . beginning-of-defun) ; default is backward-sexp
               ))

  (bind-keys :map org-mode-map
             ;; my=interactive-lambda doesn't work
             ("<menu>"      . org-latex-export-to-pdf))

  (bind-keys :map prog-mode-map
             ;; M-/  M-x hippie-expand
             ("s-Q" . dumb-jump-quick-look)
             ("s-h" . spacemacs/helm-jump-in-buffer)
             ;; previously: helm-imenu-in-all-buffers
             ("s-H" . lazy-helm/helm-imenu-in-all-buffers)
             ("s-u" . eval-buffer)
             ("s-e" . eval-last-sexp)
             )

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
       (bind-keys :map state-map
                  ("s-o" . racket-run-and-switch-to-repl)
                  ("<C-s-delete>" . my=racket-repl-clear)
                  ("M-s-d"  . my=racket-insert-fn)
                  ("M-s-p"  . my=insert-partial)
                  ("C-s-p"  . my=racket-insert-log)
                  ("C-s-\\" . my=racket-toggle-reader-comment-fst-sexp-on-line)
                  ("s-\\"   . my=racket-toggle-reader-comment-fst-sexp-on-line)))))

  (my=bind-keys-racket 'racket-mode-hook      'racket-mode-map)
  (my=bind-keys-racket 'racket-repl-mode-hook 'racket-repl-mode-map)

  ;; (bind-keys :map helm-mode-map)

  (spacemacs/set-leader-keys
    "oy" 'my=copy-to-clipboard
    "op" 'my=paste-from-clipboard)

  ;; advice, defadvice and letf shouldn't be used:
  ;; https://lists.gnu.org/archive/html/emacs-devel/2012-12/msg00146.html
  ;; Emacs 24.4 replaces this mechanism with advice-add

  ;; Difference between `evil-search-forward` and `evil-ex-search-forward`:
  ;; evil-search-forward    - wrap emacs isearch-forward
  ;; evil-ex-search-forward - invoke the evil internal search
  ;; https://emacs.stackexchange.com/a/24913

  ;; See
  ;; https://www.reddit.com/r/emacs/comments/6ewd0h/how_can_i_center_the_search_results_vertically/?utm_source=share&utm_medium=web2x
  (advice-add 'evil-ex-search-next     :after 'evil-scroll-line-to-center)
  (advice-add 'evil-ex-search-previous :after 'evil-scroll-line-to-center)
  (advice-add 'ediff-quit              :around 'my=disable-y-or-n-p)
  (advice-add 'helm-mini               :before 'my=helm-mini)

  ;; (advice-remove 'magit-stash :after)
  ;; (defun my=magit-stash-no-msg () (magit-stash ""))
  ;; (advice-add 'magit-stash :after #'my=magit-stash-no-msg)

  ;; TODO workaround for (global-set-key (kbd "C-M-k") 'kill-sexp) overridden by
  ;; layers/+misc/multiple-cursors/packages.el
  (dolist (state-map `(,evil-normal-state-map ,evil-insert-state-map))
    (bind-keys :map state-map
               ("C-M-k" . kill-sexp)))

  (dolist (state-map `(,evil-motion-state-map ,evil-visual-state-map))
    ;; Move by screen lines instead of logical (long) lines
    (bind-keys :map state-map
               ("j" . evil-next-visual-line)
               ("k" . evil-previous-visual-line)))

  (bind-keys :map evil-visual-state-map
             ("p" . my=evil-paste-after-from-0))

  ;; see also binding for <f2>
  ;; (bind-keys :map evil-normal-state-map
  ;;            ("f" . my=evil-avy-goto-char-timer)
  ;;            ("t" . my=evil-avy-goto-char-timer))

  ;; (add-to-list 'spacemacs-indent-sensitive-modes 'clojure-mode)
  ;; (add-to-list 'spacemacs-indent-sensitive-modes 'clojurescript-mode)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-boot-parameters "cider repl -s wait")
 '(evil-want-Y-yank-to-eol nil)
 '(global-display-line-numbers-mode t)
 '(lean-rootdir (format "%s/lean-3.4.1-linux/" (getenv "HOME")))
 '(magit-log-section-commit-count 25)
 '(package-archive-priorities (quote (("melpa-stable" . 1) ("melpa" . 0))))
 '(package-selected-packages
   (quote
    (org-plus-contrib org-projectile org-present org-pomodoro alert log4e gntp org-download htmlize gnuplot zop-to-char yapfify ws-butler winum which-key web-mode web-beautify volatile-highlights vimrc-mode vi-tilde-fringe uuidgen use-package unfill typed-clojure-mode transpose-frame toc-org tagedit super-save suggest sql-indent spaceline solarized-theme soft-morning-theme smeargle slim-mode scss-mode sass-mode restart-emacs rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode popwin pip-requirements persp-mode pcre2el paradox orgit org-bullets open-junk-file neotree mwim move-text mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode key-chord json-mode js2-refactor js-doc intero info+ indent-guide hy-mode hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-cider-history helm-cider helm-c-yasnippet helm-ag haskell-snippets google-translate google-this golden-ratio gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu espresso-theme emmet-mode elisp-slime-nav eclipse-theme dumb-jump drag-stuff define-word dactyl-mode cython-mode csv-mode crux company-web company-tern company-statistics company-quickhelp company-ghci company-ghc company-cabal company-anaconda column-enforce-mode coffee-mode cmm-mode clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu auto-yasnippet auto-highlight-symbol auto-compile apropospriate-theme aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((eval progn
           (let
               ((local-map
                 (or
                  (current-local-map)
                  (make-keymap))))
             (use-local-map local-map)
             (setq ufo-dir "~/dev/zark")
             (define-key local-map
               (kbd "<s-f2>")
               (lambda nil
                 (interactive)
                 (find-file
                  (concat ufo-dir "/src/zark/reasoned-schemer.clj"))))))))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
