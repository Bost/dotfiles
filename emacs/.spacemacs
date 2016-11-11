;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
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
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     auto-completion
     better-defaults
     emacs-lisp
     git
     ;; markdown
     ;; org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     ;; syntax-checking
     ;; version-control
     clojure
     sql
     vimscript
     markdown
     javascript
     ;; shell
     ;; command-log - log keystrokes
     ;; search-engine
     ;; TODO eyebrowse - window management
     ;; TODO spacemacs-layout - window management
     ;; smex ; smart M-x enhacements - recent & most used commands
     ;; gnus
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
     ;; suggest - discovering elisp fns; doesn't work because of loop-1.3
     crux super-save zop-to-char fish-mode drag-stuff helm-cider helm-cider-history
          typed-clojure-mode)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer--elpa-archives)
  ;; (push '(helm . "melpa-stable") package-pinned-packages)
  ;; (push '(helm-core . "melpa-stable") package-pinned-packages)
  ;; (push '(cider . "melpa-stable") package-pinned-packages)
  ;; (push '(clj-refactor . "melpa-stable") package-pinned-packages)
  ;; (push '(projectile . "melpa-stable") package-pinned-packages)

  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-dark
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font
   '(
     "Fira Code" :size 13 :weight normal :width normal :powerline-scale 1.1
     ;; "Source Code Pro" :size 13 :weight normal :width normal :powerline-scale 1.1
     ;; "Consolas" :size 16 :weight normal :width normal :powerline-offset 2
     ;; "Ubuntu Mono" :size 16 :weight normal :width normal :powerline-scale 1.1
     )
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
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
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
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
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; TODO dotspacemacs-line-numbers
   ;; '(:relative
   ;;   nil
   ;;   :disabled-for
   ;;   (dired-mode    ; works
   ;;    doc-view-mode ; ?
   ;;    markdown-mode ; gets overriden
   ;;    org-mode      ; gets overriden
   ;;    pdf-view-mode ; ?
   ;;    text-mode     ; gets overriden
   ;;    )
   ;;   :size-limit-kb 1000)
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; (push '(clojuredocs
  ;;         :name "Clojure Docs"
  ;;         :url "http://clojuredocs.org/clojure.core/%s")
  ;;       search-engine-alist)

  (spacemacs/toggle-menu-bar-on)

  ;; TODO Compile with gnutls support! See:
  ;; 1. /configure output
  ;; 2. e -e "(gnutls-available-p)"

  (defun hilight-line-dups ()
    (interactive)
    (let ((count  0)
          line-re)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (setq count    0
                line-re  (concat "^" (regexp-quote (buffer-substring-no-properties
                                                    (line-beginning-position)
                                                    (line-end-position)))
                                 "$"))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (if (not (re-search-forward line-re nil t))
                  (goto-char (point-max))
                (setq count  (1+ count))
                (unless (< count 2)
                  (hlt-highlight-region (line-beginning-position) (line-end-position)
                                        'font-lock-warning-face)
                  (forward-line 1)))))
          (forward-line 1)))))

  (defun close-buffer ()
    (interactive)
    (if server-buffer-clients
        (server-edit)
      (kill-this-buffer)))

  (global-set-key (kbd "s-q") 'other-window)
  (global-set-key (kbd "s-k") 'close-buffer)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "s-0") 'delete-window)
  (global-set-key (kbd "s-1") 'delete-other-windows)
  (global-set-key (kbd "<f8>") 'transpose-frame)

  ;; TODO create toggle-narrow-to-defun
  (global-set-key (kbd "s-n") 'narrow-to-defun)
  (global-set-key (kbd "s-N") 'widen)

  (defun split-other-window-and (f)
    (funcall f)
    (other-window 1))

  (defun split-other-window-below ()
    (interactive)
    (split-other-window-and 'split-window-below))

  (defun split-other-window-right ()
    (interactive)
    (split-other-window-and 'split-window-right))

  (global-set-key (kbd "s-2") 'split-other-window-below)
  (global-set-key (kbd "s-3") 'split-other-window-right)

  ;; Default theme applied at startup
  (global-set-key (kbd "s-a") 'helm-mini)
  (global-set-key (kbd "s-z") (lambda ()
                                (interactive)
                                (bs-show nil)
                                (if (not (evil-insert-state-p))
                                    (evil-insert 0))))
  ;; dired: https://danlamanna.com/forget-scp-use-dired-dwim.html
  (global-set-key (kbd "s-d") 'dired-jump)
  (global-set-key (kbd "s-D") 'spacemacs/find-dotfile)
  ;; (global-set-key (kbd "s-c") 'paredit-copy-as-kill)

  (defun sp-copy-sexp-msg ()
    (interactive)
    (sp-copy-sexp)
    (message (format "sexp (%d chars) copied to kill ring"
                     (length (car kill-ring)))))

  (defun sp-copy-back-sexp-msg ()
    (interactive)
    (let* ((point-pos (point)))
      (sp-backward-sexp)
      (sp-copy-sexp-msg)
      (goto-char point-pos)))

  (global-set-key (kbd "s-c") 'sp-copy-sexp-msg)
  (global-set-key (kbd "s-C") 'sp-copy-back-sexp-msg)
  (global-set-key (kbd "s-b") 'sp-copy-back-sexp-msg)

  ;; Move the parenthesis
  (global-set-key (kbd "M-s-<left>")  'paredit-forward-barf-sexp)
  (global-set-key (kbd "M-s-<right>") 'paredit-forward-slurp-sexp)
  (global-set-key (kbd "C-s-<left>")  'paredit-backward-slurp-sexp)
  (global-set-key (kbd "C-s-<right>") 'paredit-backward-barf-sexp)

  (global-set-key (kbd "s-;") 'spacemacs/comment-or-uncomment-lines)
  (global-set-key (kbd "s-<f1>") 'eshell)
  (global-set-key (kbd "s-p") 'helm-projectile)
  (global-set-key (kbd "s-w") 'whitespace-mode)
  (global-set-key (kbd "s-m") 'magit-status)
  ;; search only in certain file-types:
  ;; 1. ag --list-file-types
  ;; 2. search only in .el files: TextToFind -G\.el$
  (global-set-key (kbd "<f3>") 'helm-ag)
  (global-set-key (kbd "s-f") 'helm-find-files)
  ;; ("s-F"   . helm-find-files)

  (global-set-key (kbd "<C-up>") 'xah-backward-block)
  (global-set-key (kbd "<C-down>") 'xah-forward-block)
  (global-set-key (kbd "<C-prior>") 'hs-hide-block)
  (global-set-key (kbd "<C-next>") 'hs-show-block)
  ;; (global-set-key (kbd "<C-M-prior>") 'hs-toggle-hiding)
  (global-set-key (kbd "<C-M-prior>") 'hs-hide-all)
  (global-set-key (kbd "<C-M-next>") 'hs-show-all)

  ;; (global-set-key (kbd "<C-M-right>") 'sp-forward-sexp)
  (global-set-key (kbd "<C-M-right>") 'forward-paragraph)  ; end-of-defun
  (global-set-key (kbd "<C-M-left>")  'backward-paragraph) ; beginning-of-defun

  (global-set-key (kbd "<C-M-delete>") 'kill-sexp)
  (global-set-key (kbd "<C-M-backspace>") 'backward-kill-sexp)
  (global-set-key (kbd "<s-backspace>") 'paredit-backward-kill-word)
  (global-set-key (kbd "<s-delete>") 'paredit-forward-kill-word)

  (global-set-key (kbd "s-SPC") 'evil-search-highlight-persist-remove-all)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring) ; replaces evil-paste-pop
  (global-set-key (kbd "s-g") 'helm-google-suggest) ; ("s-g" . google-this)
  (global-set-key (kbd "s-8") 'er/expand-region)
  (global-set-key (kbd "s-*") 'er/contract-region)

  (global-set-key (kbd "s-y") 'avy-goto-line)

  (setq create-lockfiles nil) ; do not create .# lockfiles
  (setq vc-follow-symlinks t) ; auto follow symbolic links
  ;; disable mouse support in X11 terminals - enables copy/paste with mouse
  (xterm-mouse-mode -1)

  (global-set-key (kbd "<f2>")   'evil-avy-goto-char)
  (global-set-key (kbd "C-a")    'evil-avy-goto-char)
  (global-set-key (kbd "<M-f2>") 'avy-goto-subword-1)
  (global-set-key (kbd "<C-f2>") 'avy-goto-line)
  ;; (global-set-key (kbd "s-j") 'avy-goto-char)

  (global-set-key (kbd "<C-mouse-5>")
                  (lambda () (interactive) (message "zoom-out")))
  (global-set-key (kbd "<C-mouse-4>")
                  (lambda () (interactive) (message "zoom-out")))
  (global-set-key (kbd "<menu>")
                  (lambda () (interactive) (message "context-menu")))

  ;; fd - evil-escape from insert state and everything else

  (global-set-key (kbd "s-i") ;; occurences - function scope
                  (lambda () (interactive) (iedit-mode 0)))
  (global-set-key (kbd "s-I") 'iedit-mode) ;; all occurences

  ;; (global-set-key (kbd"s-i")  'spacemacs/enter-ahs-forward)
  (global-set-key (kbd "s-h") 'helm-imenu)
  (global-set-key (kbd "<f12>") 'undo-tree-visualize)

  ;; ("<S-delete>"      . kill-region)
  ;; ("<C-s-backspace>" . kill-line-backward)
  (global-set-key (kbd "<C-s-delete>") 'kill-line)

  (global-set-key (kbd "s-l") 'spacemacs/resume-last-search-buffer)

  (defun toggle-large-file-setting ()
    (interactive)
    (let* ((msg "large-file-settings"))
      (cond
       ((not linum-mode)
        (progn
          ;; fontification is only deferred while there is input pending
          (setq jit-lock-defer-time 0)
          (spacemacs/toggle-line-numbers-on)
          (buffer-enable-undo)
          (font-lock-mode 1)
          (if (> (buffer-size) (* 1024 1024))
              (message (format "WARN %s disabled on a large file!" msg))
            (message (format "%s disabled" msg)))))

       (t ;; default
        (progn
          (spacemacs/toggle-line-numbers-off)
          (buffer-disable-undo)
          (font-lock-mode -1)
          ;; fontification is not deferred.
          (setq jit-lock-defer-time nil)
          (message (format "%s enabled" msg)))))))

  ;; (add-hook 'find-file-hook ''toggle-large-file-setting)
  (global-set-key (kbd "s-L") 'toggle-large-file-setting)

  (use-package fish-mode
    :config
    (add-hook 'fish-mode-hook #'paredit-mode))

  (use-package zop-to-char
    :ensure t
    :bind (("M-z" . zop-up-to-char)
           ("M-Z" . zop-to-char)))

  (use-package drag-stuff
    :bind (("<M-up>"   . drag-stuff-up)
           ("<M-down>" . drag-stuff-down)))

  (use-package crux
    :ensure t
    :bind
    (("C-c d"           . crux-duplicate-current-line-or-region)
     ("<C-s-down>"      . crux-duplicate-current-line-or-region)
     ("C-c t"           . crux-transpose-windows)
     ("<C-s-backspace>" . crux-kill-line-backwards)))

  (defun clojure-insert-sexp (str-sexp n-chars-back)
    (insert str-sexp)
    (left-char n-chars-back))

  (defun elisp-insert-message ()
    (interactive)
    (clojure-insert-sexp "(message (format \"\"))" 3))

  (use-package emacs
    :bind (("C-s-m" . elisp-insert-message)
           ("s-e"   . eval-last-sexp)))

  (use-package clojure-mode
    :config
    (add-hook 'clojure-mode-hook 'typed-clojure-mode)
    (add-hook 'clojure-mode-hook (lambda () (prettify-symbols-mode)))
    (defun clj-cmt-uncmt-line-sexp ()
      (interactive)
      (let* ((point-pos1 (point)))
        (evil-insert-line 0)
        (let* ((point-pos2 (point))
               (cmtstr "#_")
               (cmtstr-len (length cmtstr))
               (line-start (buffer-substring-no-properties
                            point-pos2 (+ point-pos2 cmtstr-len))))
          (if (string= cmtstr line-start)
              (delete-char cmtstr-len)
            (insert cmtstr))
          (goto-char point-pos1))))
    (bind-keys :map clojure-mode-map
               ;; ("s-_" . clojure-ignore-next-form)
               ;; on the german keyboard the '#' is next to Enter
               ("s-i" . cljr-rename-symbol)
               ("C-s-\\" . (lambda () (interactive) (insert "#_")))
               ("s-\\" . clj-cmt-uncmt-line-sexp)))

  (use-package super-save ;; better auto-save-mode
    :ensure t
    :config
    (super-save-mode +1))

  (defun progress-report (orig-fun &rest args)
    (let ((progress-reporter
           (make-progress-reporter
            (format "Evaluating (%s %s)..." orig-fun args))))
      (let ((res (apply orig-fun args)))
        (progress-reporter-done progress-reporter)
        res)))

  (advice-add 'eval-buffer :around #'progress-report)
  ;; (advice-remove 'eval-buffer #'progress-report)
  (global-set-key (kbd "s-u") 'eval-buffer)

  (global-set-key (kbd "s-.") 'spacemacs/jump-to-definition)
  ;; (global-set-key (kbd "s-,") 'evil-jump-backward)
  ;; (global-set-key (kbd "s-,") 'cider-pop-back)
  (global-set-key (kbd "s-,") 'dumb-jump-back)

  (use-package cider
      ;; :init
      ;; (use-package helm-cider :ensure t :config (helm-cider-mode 1))
      :config
      (setq cider-repl-use-pretty-printing t)
      (add-hook 'cider-mode-hook #'eldoc-mode)
      (add-hook 'cider-repl-mode-hook #'eldoc-mode)
      (add-hook 'cider-repl-mode-hook #'paredit-mode)
      (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
      ;; (setq gui-elements 1) ; because of CIDER menu
      (bind-keys :map cider-repl-mode-map
                 ("<s-delete>" . cider-repl-clear-buffer)
                 ("s-j" . cider-format-defun)
                 ("s-e" . cider-eval-last-sexp)
                 ("s-x" . cider-switch-to-last-clojure-buffer)
                 ;; invoke from *.clj buffer
                 ("s-M" . main-a)
                 ("s-S" . main-s))
      (bind-keys :map cider-mode-map
                 ("<C-M-right>" . end-of-defun)       ; forward-paragraph
                 ("<C-M-left>"  . beginning-of-defun) ; backward-paragraph
                 ("s-j"         . cider-format-defun)
                 ("s-x"         . cider-switch-to-repl-buffer)
                 ("s-e"         . cider-eval-last-sexp))

      (defun figwheel-cider ()
        (interactive)
        (cider-interactive-eval "(use 'figwheel-sidecar.repl-api)")
        (cider-interactive-eval "(start-figwheel!)")
        (cider-interactive-eval "(cljs-repl)")
        ;; TODO (rename-buffer "*figwheel-cider*")
        (if (not (evil-insert-state-p))
            (evil-insert 0)))

      (defun cider-save-and-load-current-buffer ()
        (interactive)
        (when (buffer-modified-p)
          (save-buffer))
        (cider-load-file (buffer-file-name))
        ;; (cider-switch-to-relevant-repl-buffer nil)
        )

      (defun clojure-insert-println ()
        (interactive)
        (clojure-insert-sexp "(println \"\")" 2))

      ;; TODO remove this if cljr-introduce-let works as expected
      ;; (defun clojure-insert-let ()
      ;;   (interactive)
      ;;   (clojure-insert-sexp "(let [])" 2))

      (defun clojure-insert-for ()
        (interactive)
        (clojure-insert-sexp "(for [])" 2))

      (defun clojure-insert-defn ()
        (interactive)
        (clojure-insert-sexp "(defn [])" 3))

      (defun clojure-insert-doseq ()
        (interactive)
        (clojure-insert-sexp "(doseq [])" 2))

      (defun clojure-insert-do ()
        (interactive)
        (clojure-insert-sexp "(do )" 1))

      (defun clj-cmt-uncmt-line-sexp ()
        (interactive)
        (evil-insert-line 0)
        (let* ((cmtstr "#_")
               (cmtstr-len (length cmtstr))
               (point-pos (point))
               (line-start (buffer-substring-no-properties
                            point-pos (+ point-pos cmtstr-len))))
          (if (string= cmtstr line-start)
              (delete-char cmtstr-len)
            (insert cmtstr))))

      (setq cider-cljs-lein-repl
            "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

      :bind (;; lambdas are not supported
             ("s-x"   . cider-switch-to-repl-buffer)
             ("<s-insert>" . clojure-insert-println)
             ;; (bind-key "C-s-p" 'clojure-insert-println)
             ("C-s-p" . clojure-insert-println)
             ;; ("C-s-l" . clojure-insert-let)
             ("C-s-l" . cljr-introduce-let)
             ("C-s-f" . clojure-insert-for)
             ("C-s-n" . clojure-insert-defn)
             ("C-s-s" . clojure-insert-doseq)
             ("C-s-d" . clojure-insert-do)
             ;; ("s-x"   . cider-switch-to-last-clojure-buffer)
             ("s-t"   . cider-test-run-tests)
             ("C-s-j" . cider-jack-in)
             ;; ("s-r"   . cider-eval-last-expression-in-repl)
             ("M-s-l" . cider-save-and-load-current-buffer)
             ("M-s-n" . cider-repl-set-ns)
             ("s-t"   . cider-test-run-tests)

             ;; TODO see global-set-key settings
             ;; ("s-."   . cider-find-var)
             ;; ("s-,"   . cider-pop-back)
             ;; TODO s-M does not work in REPL buffer
             ("s-o"   . cider-clear-compilation-highlights)

             ;; BUG: "<s-kp-insert>" "<C-insert>" are the same keys Uhg?

             ("<s-kp-insert>" . zark-symbols)
             ("<s-kp-0>"      . zark-symbols)
             ("s-'"           . zark-symbols)
             ;; (unbind-key "<C-insert>")
             ;; ("<C-insert>"    . typed-unicode-symbols)

             ;; invoke from *.clj buffer
             ("s-M" . main-a)
             ("s-A" . main-a)
             ("s-S" . main-s)
             ("s-U" . main-u)))

  (defun copy-to-clipboard ()
    "Copies selection to x-clipboard."
    (interactive)
    (if (display-graphic-p)
        (progn
          (message "Yanked region to x-clipboard!")
          (call-interactively 'clipboard-kill-ring-save))
      (if (region-active-p)
          (progn
            (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
            (message "Yanked region to clipboard!")
            (deactivate-mark))
        (message "No region active; can't yank to clipboard!"))))

  (defun paste-from-clipboard ()
    "Pastes from x-clipboard."
    (interactive)
    (if (display-graphic-p)
        (progn
          (clipboard-yank)
          (message "graphics active"))
      (insert (shell-command-to-string "xsel -o -b"))))
  (evil-leader/set-key "o y" 'copy-to-clipboard)
  (evil-leader/set-key "o p" 'paste-from-clipboard)

  (defun fabricate-subst-cmd (&optional arg)
    "Place prepared subst command to the echo area.
Example 1.:
        :%s/\<\>//gc     - moves the point between '\<' and '\>'
Example 2.:
        :%s/fox/fox/gc   - moves the point after first 'x'"
    (interactive "p")
    (sp-copy-sexp)
    (evil-normal-state)
    (let* (;; Example 1.:
           ;; (sexp-str "%s/\\<\\>//gc")
           ;; (offset 6)
           ;;
           ;; Example 2.:
           (search-regex (format "%s" (car kill-ring)))
           (replace-regex (format "%s" (car kill-ring)))
           (sexp-str (format "%%s/%s/%s/gc" search-regex replace-regex))
           (offset (+ (length search-regex) 4)) ;; 4 means: jump to the 2nd slash
           )
      ;; (cons .. offset) moves the point
      (evil-ex (cons sexp-str offset))))
  (global-set-key (kbd "s-:") 'fabricate-subst-cmd)

  ;; Center buffer on search-next
  (defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
    (evil-scroll-line-to-center (line-number-at-pos)))

  ;; Center buffer on search-next
  (defadvice evil-search-next (after advice-for-evil-search-next activate)
    (evil-scroll-line-to-center (line-number-at-pos)))

  ;; Center buffer on search-forward
  (defadvice evil-search-forward (after advice-for-evil-search-forward activate)
    (evil-scroll-line-to-center (line-number-at-pos)))

  ;; Move by screen lines instead of logical (long) lines
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  ;; Also in visual mode
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)

  ;; (when (window-system) (set-default-font "Fira Code"))
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 ;; (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                 (91 . ".\\(?:]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (119 . ".\\(?:ww\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                 )
               ))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring]))))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-archive-priorities (quote (("melpa-stable" . 1) ("melpa" . 0))))
 '(package-selected-packages
   (quote
    (pretty-lambdada evil-surround yasnippet clj-refactor projectile spacemacs-theme help-fns+ evil-mc inflections with-editor sql-indent typed-clojure-mode helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-c-yasnippet helm-ag helm helm-core helm-cider-history helm-cider vimrc-mode mmm-mode markdown-toc markdown-mode hide-comnt gh-md dactyl-mode helm-company cider undo-tree uuidgen toc-org org-plus-contrib org-bullets mwim link-hint git-link eyebrowse evil-visual-mark-mode evil-unimpaired evil-ediff dumb-jump f column-enforce-mode clojure-snippets web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc company-tern dash-functional tern coffee-mode)))
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
