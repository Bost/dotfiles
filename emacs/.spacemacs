;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

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
     haskell
     csv
     python
     ;; (java :variables
     ;;       eclim-eclipse-dirs "~/eclipse-java-neon"
     ;;       eclim-executable "~/eclipse-java-neon/eclim")
     html
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     (auto-completion :variables auto-completion-enable-help-tooltip t)
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
   dotspacemacs-additional-packages
   '(
     ;; (yasnippet :location ;; local
     ;;            (recipe :fetcher github :repo "Bost/yasnippet"
     ;;                    ;; :min-version "1"
     ;;                    ))
     ;; send files marked in dired via MTP to Android
     ;; dired-mtp     ; not found
     ;; android-mode  ; doesn't work
     key-chord
     suggest ;; discover elisp fns
     crux super-save zop-to-char fish-mode drag-stuff helm-cider
     helm-cider-history transpose-frame typed-clojure-mode
     google-this super-save crux
     )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(
     tern ;; avoid "tern binary not found!"
     anaconda-mode ;; for python - needs working proxy
     )
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
  (push '("melpa-stable" . "stable.melpa.org/packages/")
        configuration-layer--elpa-archives)
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
   dotspacemacs-themes
   '(
     ;; dark themes
     solarized-dark spacemacs-dark leuven zenburn
     ;; light themes
     default tsdh-light dichromacy apropospriate-light espresso soft-morning
     eclipse spacemacs-light solarized-light
     )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
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
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   dotspacemacs-line-numbers
   '(:relative nil
     :disabled-for-modes dired-mode    ; works
                         doc-view-mode ; ?
                         markdown-mode ; gets overriden
                         org-mode      ; gets overriden
                         pdf-view-mode ; ?
                         text-mode     ; gets overriden!
     :size-limit-kb 1000)
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
  (global-prettify-symbols-mode +1)

  (setq
   frame-title-format "%f - Emacs" ; 'path/to/file' in title bar; %b only 'file'
   bookmark-default-file "~/dev/dotfiles/emacs/bookmarks"
   ;; Hotfix of "magit ediff on unstaged file leads to emacs freeze. #4730"
   ediff-window-setup-function 'ediff-setup-windows-default

   ;; Fix projectile-regenerate-tags: ctags: invalid option -- ’e’
   ;; See https://github.com/bbatsov/projectile/issues/133
   projectile-tags-command "ctags-exuberant -Re -f \"%s\" %s"

   create-lockfiles nil ;; do not create .# lockfiles
   vc-follow-symlinks t ;; auto follow symbolic links
   browse-url-browser-function 'browse-url-chrome
                               ;; '(("wikipedia\\.org" . browse-url-firefox)
                               ;;   ("github" . browse-url-chromium)
                               ;;   ("thefreedictionary\\.com" . eww-browse-url)
                               ;;   ("." . browse-url-default-browser))
   )

  (defun what-face (pos)
    (interactive "d")
    (let ((face (or (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face))))
      (if face (message "Face: %s" face) (message "No face at %d" pos))))

  (defun hilight-line-dups ()
    (interactive)
    (let ((count 0)
          line-re)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (setq count 0
                line-re (concat "^" (regexp-quote
                                     (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position)))
                                "$"))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (if (not (re-search-forward line-re nil t))
                  (goto-char (point-max))
                (setq count (1+ count))
                (unless (< count 2)
                  (hlt-highlight-region (line-beginning-position)
                                        (line-end-position)
                                        'font-lock-warning-face)
                  (forward-line 1)))))
          (forward-line 1)))))

  (defun close-buffer ()
    (interactive)
    (if server-buffer-clients
        (server-edit)
      (kill-this-buffer)))

  (defun my/rudekill-matching-buffers (regexp &optional internal-too)
    "Kill - WITHOUT ASKING - buffers whose name matches the specified REGEXP.
See the `kill-matching-buffers` for grateful killing. The optional 2nd argument
indicates whether to kill internal buffers too.

Returns the count of killed buffers."
    (let* ((buffers (remove-if-not
                     (lambda (buffer)
                       (let ((name (buffer-name buffer)))
                         (and name (not (string-equal name ""))
                              (or internal-too (/= (aref name 0) ?\s))
                              (string-match regexp name))))
                     (buffer-list))))
      (mapc 'kill-buffer buffers)
      (length buffers)))

  (defun my/kill-matching-buffers-rudely (regexp &optional internal-too)
    "Kill - WITHOUT ASKING - buffers whose name matches the specified REGEXP.
See the `kill-matching-buffers` for grateful killing. The optional 2nd argument
indicates whether to kill internal buffers too.

Returns a message with the count of killed buffers."
    (interactive "sKill buffers matching this regular expression: \nP")
    (message
     (format "%d buffer(s) killed."
             (my/rudekill-matching-buffers regexp internal-too))))

  (defun kill-all-magit-buffers ()
    "Kill all Magit buffers."
    (interactive)
    ;; (my/kill-matching-buffers-rudely "\*magit: .*\\|\*magit-.*")
    (save-excursion
      (let ((count 0))
        (dolist (buffer (buffer-list))
          (set-buffer buffer)
          (when (find major-mode '(magit-status-mode
                                   magit-log-mode
                                   magit-diff-mode
                                   magit-revision-mode
                                   magit-stash-mode
                                   magit-process-mode))
            (setq count (1+ count))
            (kill-buffer buffer)))
        (message "Killed %i Magit buffer(s)." count))))

  (defun buffer-mode (buffer-or-string)
    "Returns the major mode associated with a buffer.
Example: (buffer-mode (current-buffer))"
    (with-current-buffer buffer-or-string
      major-mode))

  (defun kill-unwanted-buffers ()
    (interactive)
    (save-excursion
      (let ((count 0))
        (dolist (buffer (buffer-list))
          (set-buffer buffer)
          ;; find out buffer's major mode: (message "%s" major-mode)
          (when (find major-mode
                      '(magit-status-mode
                        magit-log-mode
                        magit-diff-mode
                        magit-revision-mode
                        magit-stash-mode
                        magit-process-mode
                        bs-mode ; *buffer-selection*
                        ;; *package-build-checkout* is in fundamenatal-mode
                        ;; *cider-refresh-log* is in fundamenatal-mode
                        cider-browse-ns-mode  ; for *cider-ns-browser*
                        cider-stacktrace-mode ; for *cider-error*
                        cider-docview-mode    ; for *cider-doc*
                        cider-inspector-mode  ; for *cider-inspect*
                        help-mode             ; for *Help*
                        dired-mode))
            (setq count (1+ count))
            (kill-buffer buffer)))
        (message "Buffer(s) killed: %i" count))))

  (global-set-key (kbd "s-K") 'kill-unwanted-buffers)

  (defun kill-all-dired-buffers ()
    "Kill all dired buffers."
    (interactive)
    (save-excursion
      (let ((count 0))
        (dolist (buffer (buffer-list))
          (set-buffer buffer)
          (when (equal major-mode 'dired-mode)
            (setq count (1+ count))
            (kill-buffer buffer)))
        (message "Killed %i dired buffer(s)." count))))

  (global-set-key (kbd "s-C-K") 'kill-all-dired-buffers)

  (global-set-key (kbd "s-R") 'spacemacs/rename-current-buffer-file)

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
  (defun flash-active-buffer ()
    (interactive)
    (run-at-time "100 millisec" nil
                 (lambda (remap-cookie)
                   (face-remap-remove-relative remap-cookie))
                 (face-remap-add-relative 'default 'flash-active-buffer-face)))

  (global-set-key (kbd "s-q") (lambda ()
                                (interactive)
                                (other-window 1)
                                (flash-active-buffer)))

  (global-set-key (kbd "s-k") 'close-buffer)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "s-0") 'delete-window)
  (global-set-key (kbd "s-1") 'delete-other-windows)
  (global-set-key (kbd "<f8>") 'transpose-frame)
  (global-set-key (kbd "<M-f8>") 'spacemacs/rotate-windows-forward)


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

  ;; (global-set-key (kbd "s-2") 'split-other-window-below)
  ;; (global-set-key (kbd "s-3") 'split-other-window-right)
  (global-set-key (kbd "s-2") 'split-window-below) ;; SPC w -
  (global-set-key (kbd "s-3") 'spacemacs/layout-double-columns) ; SPC w 2

  ;; Default theme applied at startup
  (global-set-key (kbd "s-a") 'helm-mini)
  (global-set-key (kbd "s-z") (lambda ()
                                (interactive)
                                (bs-show nil)
                                (if (not (evil-insert-state-p))
                                    (evil-insert 0))))
  ;; dired: https://danlamanna.com/forget-scp-use-dired-dwim.html
  (global-set-key (kbd "s-D") 'dired-jump)

  (defun sp-copy-sexp-msg ()
    (interactive)
    (sp-copy-sexp)
    (let* ((sexp (car kill-ring))
           (sexp-lines (split-string sexp "\n"))
           (sexp-len (length sexp))
           (cnt-sexp-lines (length sexp-lines))
           (fst-line (car sexp-lines))
           (fst-line-len (length fst-line))
           (maxchars 40))
      (message
       (format "sexp (%d chars, %d lines) copied to kill-ring: %s..."
               sexp-len
               cnt-sexp-lines
               fst-line
               ;; (or (>= fst-line-len maxchars) (> (length sexp-lines) 1))
               ;; (if (or (>= fst-line-len maxchars) (> (length sexp-lines) 1))
               ;;     (concat (subseq fst-line 0 (- maxchars 3)) "...")
               ;;   fst-line)
               ))))

  (defun sp-copy-back-sexp-msg ()
    (interactive)
    (let* ((point-pos (point)))
      (sp-backward-sexp)
      (sp-copy-sexp-msg)
      (goto-char point-pos)))

  (global-set-key (kbd "s-c") 'sp-copy-sexp-msg)
  (global-set-key (kbd "s-b") 'sp-copy-back-sexp-msg)
  (global-set-key (kbd "s-B") 'helm-filtered-bookmarks)
  (global-set-key (kbd "<f9>") 'helm-filtered-bookmarks)
  (global-set-key (kbd "<f11>") 'bookmark-set)

  (defun select-inner (vi-str)
    (evil-normal-state)
    (execute-kbd-macro vi-str))

  (global-set-key (kbd "s-<") (lambda () (interactive) (select-inner "vi<")))
  (global-set-key (kbd "s-[") (lambda () (interactive) (select-inner "vi[")))
  (global-set-key (kbd "s-(") (lambda () (interactive) (select-inner "vi(")))
  (global-set-key (kbd "s-{") (lambda () (interactive) (select-inner "vi{")))
  (global-set-key (kbd "s-\"") (lambda () (interactive) (select-inner "vi\"")))

  (defun disable-y-or-n-p (orig-fun &rest args)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
      (apply orig-fun args)))
  (advice-add 'ediff-quit :around #'disable-y-or-n-p)

  (global-set-key (kbd "<s-print>")
                  (lambda (&optional arg) "ediff-buffers-left-right"
                    (interactive "p")
                    (ediff-buffers (buffer-name) ;; gives the current buffer
                                   (buffer-name (other-window 1)))))

  ;; Move the parenthesis - see SPC k b/B/f/F
  (global-set-key (kbd "M-s-<left>")  'sp-forward-barf-sexp)
  (global-set-key (kbd "M-s-<right>") 'sp-forward-slurp-sexp)
  (global-set-key (kbd "C-s-<left>")  'sp-backward-slurp-sexp)
  (global-set-key (kbd "C-s-<right>") 'sp-backward-barf-sexp)

  (global-set-key (kbd "s-;") 'spacemacs/comment-or-uncomment-lines)
  (global-set-key (kbd "s-<f1>") 'eshell)
  (global-set-key (kbd "s-p") 'helm-projectile)
  (global-set-key (kbd "s-w")
                  (lambda ()
                    (interactive)
                    (whitespace-mode 'toggle)
                    (message (format "s-n / s-N : narrow-to-defun / widen"))))

  (global-set-key (kbd "s-m") 'magit-status)

  ;; search only in certain file-types:
  ;; 1. ag --list-file-types
  ;; 2. search only in .el files: TextToFind -G\.el$
  ;; (global-set-key (kbd "<f3>") 'helm-ag)

  ;; See also: SPC s
  (global-set-key (kbd "<f3>") 'spacemacs/helm-project-smart-do-search)
  (global-set-key (kbd "<M-f3>") (lambda ()
                                   (interactive)
                                   (spacemacs/helm-project-smart-do-search t)))

  (global-set-key (kbd "s-f") 'helm-find-files)
  (global-set-key (kbd "s-F") 'recentf-open-files)
  ;; ("s-F"   . helm-find-files)

  (global-set-key (kbd "<C-up>") 'xah-backward-block)
  (global-set-key (kbd "<C-down>") 'xah-forward-block)
  (global-set-key (kbd "<C-prior>") 'hs-hide-block) ; pg-up
  (global-set-key (kbd "<C-next>") 'hs-show-block)  ; pg-down
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
  (global-set-key (kbd "s-g") 'helm-google-suggest)
  (global-set-key (kbd "s-G") 'google-this)
  (global-set-key (kbd "s-8") 'er/expand-region)
  ;; TODO see https://github.com/joshwnj `er/contract-region`
  ;; (global-set-key (kbd "s-*") 'er/contract-region)

  ;; disable mouse support in X11 terminals - enables copy/paste with mouse
  (xterm-mouse-mode -1)

  (defun my-evil-avy-goto-char ()
    (interactive)
    (evil-avy-goto-char)
    (message (format "evil-avy-goto-char: SPC j j, <f2>, <s-tab>, s-/")))

  (global-set-key (kbd "<f2>")    'my-evil-avy-goto-char)
  (global-set-key (kbd "s-/")     'my-evil-avy-goto-char)

  (global-set-key (kbd "<s-tab>") (lambda ()
                    (interactive)
                    (spacemacs/alternate-buffer)
                    (message (format
                              "spacemacs/alternate-buffer: SPC TAB, <s-tab>"))))

  ;; TODO evaluate: paste copied text multiple times
  (defun evil-paste-after-from-0 ()
    (interactive)
    (let ((evil-this-register ?0))
      (call-interactively 'evil-paste-after)))

  (define-key evil-visual-state-map "p" 'evil-paste-after-from-0)


  ;; TODO make it run under "t"
  ;; (global-set-key (kbd "s-t")    'evil-avy-goto-char
  ;;                                 ;; This doesn't work
  ;;                                 ;; (lambda ()
  ;;                                 ;;   (interactive)
  ;;                                 ;;   (if (evil-normal-state-p)
  ;;                                 ;;       (evil-avy-goto-char)))
  ;;                 )

  (defun my-avy-goto-line ()
    (interactive)
    (avy-goto-line)
    (message (format "avy-goto-line: SPC j l, M-m j l, <C-f2>, C-s-/")))

  (global-set-key (kbd "<C-f2>") 'my-avy-goto-line)
  (global-set-key (kbd "C-s-/")  'my-avy-goto-line)

  (global-set-key (kbd "<C-mouse-5>")
                  (lambda () (interactive) (message "zoom-out")))
  (global-set-key (kbd "<C-mouse-4>")
                  (lambda () (interactive) (message "zoom-out")))
  (global-set-key (kbd "<menu>")
                  (lambda () (interactive) (message "context-menu")))

  ;; fd - evil-escape from insert state and everything else

  (global-set-key (kbd "s-I") ;; occurences - function scope
                  (lambda () (interactive) (iedit-mode 0)))
  (global-set-key (kbd "s-i") 'iedit-mode) ;; all occurences

  ;; (global-set-key (kbd"s-i")  'spacemacs/enter-ahs-forward)
  (global-set-key (kbd "s-h") 'helm-imenu)
  (global-set-key (kbd "<f12>") 'undo-tree-visualize)

  ;; ("<S-delete>"      . kill-region)
  ;; ("<C-s-backspace>" . kill-line-backward)
  (global-set-key (kbd "<C-s-delete>") 'kill-line) ; super-key
  (global-set-key (kbd "<C-S-delete>") 'kill-line) ; shift-key

  (global-set-key (kbd "s-l") 'spacemacs/resume-last-search-buffer)

  (defun my/evil-select-pasted ()
    (interactive)
    (let ((start-marker (evil-get-marker ?[))
                        (end-marker (evil-get-marker ?])))
      (evil-visual-select start-marker end-marker)))

  ;; (evil-leader/set-key "v" 'my/evil-select-pasted)

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

  (defun my/org-mode-hook ()
    (progn
      ;; do not set "TODO"; use default key bindings
      (define-key org-mode-map (kbd "<S-right>") nil)
      (define-key org-mode-map (kbd "<S-left>") nil)
      (define-key org-mode-map (kbd "<C-S-right>") nil)
      (define-key org-mode-map (kbd "<C-S-left>") nil)

      ;; Don't increase the org-level header text height
      (dolist (face '(org-level-1
                      org-level-2
                      org-level-3
                      org-level-4
                      org-level-5))
        (set-face-attribute face nil :weight 'semi-bold :height 1.0))))

  (add-hook 'org-mode-hook 'my/org-mode-hook)
  ;; (eval-after-load 'org ; alternative invocation
  ;;   (progn
  ;;     (define-key org-mode-map (kbd "<S-right>") nil)
  ;;     (define-key org-mode-map (kbd "<S-left>") nil)))

  ;; jump like f/t in vim; TODO integrate zop-to-char with 'y' in evil
  ;; zop-up-to-char works as zop-to-char but stop just before target
  (global-set-key (kbd "M-z") 'zop-up-to-char)
  (global-set-key (kbd "M-Z") 'zop-to-char)

  ;; spacemacs orig fns don't drag
  (global-set-key (kbd "M-<up>")
                  ;; 'spacemacs/move-text-transient-state/move-text-up
                  'drag-stuff-up)
  (global-set-key (kbd "M-<down>")
                  ;; 'spacemacs/move-text-transient-state/move-text-down
                  'drag-stuff-down)

  (use-package crux ;; Coll of Ridiculously Useful eXtensions bbatsov/crux
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
           ("s-d"   . eval-defun)
           ("s-e"   . eval-last-sexp)))

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

  (defun hs-clojure-hide-namespace-and-folds ()
    "Hide the first (ns ...) expression in the file, and also all
the (^:fold ...) expressions."
    (interactive)
    (hs-life-goes-on
     (save-excursion
       (goto-char (point-min))
       (when (ignore-errors (re-search-forward "^(ns "))
         (hs-hide-block))

       (while (ignore-errors (re-search-forward "\\^:fold"))
         (hs-hide-block)
         (next-line)))))

  (defun hs-clojure-mode-hook ()
    (interactive)
    (hs-minor-mode 1)
    (hs-clojure-hide-namespace-and-folds))

  (use-package clojure-mode
    :config
    (add-hook 'clojure-mode-hook 'typed-clojure-mode)

    ;; see (global-prettify-symbols-mode +1)
    ;; (add-hook 'clojure-mode-hook (lambda () (prettify-symbols-mode)))

    ;; 1st invocation (clojure mode cold start) doesn't work
    (add-hook 'clojure-mode-hook 'hs-clojure-mode-hook)
    (bind-keys :map clojure-mode-map
               ;; followind 3 bindings are same as in cider
               ;; on the german keyboard the '#' is next to Enter
               ("s-i" . cljr-rename-symbol)
               ("C-s-\\" . (lambda () (interactive) (insert "#_")))
               ("s-\\" . clj-cmt-uncmt-line-sexp)))

  (use-package super-save ;; better auto-save-mode
    :config (super-save-mode +1))

  ;; (defun progress-report (orig-fun &rest args)
  ;;   (let ((progress-reporter
  ;;          (make-progress-reporter
  ;;           (format "Evaluating (%s %s)..." orig-fun args))))
  ;;     (let ((res (apply orig-fun args)))
  ;;       (progress-reporter-done progress-reporter)
  ;;       res)))
  ;; (advice-add 'eval-buffer :around #'progress-report)
  ;; (advice-remove 'eval-buffer #'progress-report)
  (global-set-key (kbd "s-u") 'eval-buffer)

  (global-set-key (kbd "s-.") 'spacemacs/jump-to-definition)
  ;; (global-set-key (kbd "s-,") 'evil-jump-backward) ;; C-o: evil-jump-backward
  ;; (global-set-key (kbd "s-,") 'cider-pop-back)
  (global-set-key (kbd "s-,") 'dumb-jump-back)
  (global-set-key (kbd "<pause>") 'goto-last-change)
  (global-set-key (kbd "<s-pause>") 'goto-last-change-reverse)
  (global-set-key (kbd "s-J") 'evil-join)

  (defun smarter-move-beginning-of-line (arg)
    "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
    (interactive "^p")
    (setq arg (or arg 1))

    ;; Move lines first
    (when (/= arg 1)
      (let ((line-move-visual nil))
        (forward-line (1- arg))))

    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (move-beginning-of-line 1))))

  ;; remap C-a/<home> to `smarter-move-beginning-of-line'
  (global-set-key [remap move-beginning-of-line]
                  'smarter-move-beginning-of-line)

  (defun switch-to-previous-buffer ()
    "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))

  ;; Max time delay between two key presses to be considered a key chord
  ;; (setq key-chord-two-keys-delay 0.1) ; default 0.1
  ;; Max time delay between two presses of the same key to be considered a key chord.
  ;; Should normally be a little longer than `key-chord-two-keys-delay'.
  ; (setq key-chord-one-key-delay 0.2) ; default 0.2
  (key-chord-define-global "KK" 'switch-to-previous-buffer)

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
                 ;; followind 3 bindings are same as in clojure-mode
                 ;; on the german keyboard the '#' is next to Enter
                 ("s-i" . cljr-rename-symbol)
                 ("C-s-\\" . (lambda () (interactive) (insert "#_")))
                 ("s-\\" . clj-cmt-uncmt-line-sexp)

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
                 ("s-d"         . cider-eval-defun-at-point)
                 ("s-j"         . cider-format-defun)
                 ("s-x"         . cider-switch-to-repl-buffer)
                 ("s-X"         . (lambda ()
                                    (interactive)
                                    (cider-switch-to-repl-buffer)
                                    (cider-figwheel-repl)))
                 ("s-e"         . cider-eval-last-sexp))

      (defun cider-figwheel-repl ()
        (interactive)
        (save-some-buffers)
        (with-current-buffer (cider-current-repl-buffer)
          (goto-char (point-max))
          (insert "(require 'figwheel-sidecar.repl-api)
;; start-figwheel can be repeatedly called (is idempotent)
(figwheel-sidecar.repl-api/start-figwheel!)
(figwheel-sidecar.repl-api/cljs-repl)")
          (cider-repl-return)
          ;; TODO (rename-buffer "*figwheel-cider*")
          (if (not (evil-insert-state-p))
              (evil-insert 0))))

      (defun cider-save-and-load-current-buffer ()
        (interactive)
        (when (buffer-modified-p)
          (save-buffer))
        (cider-load-file (buffer-file-name))
        ;; (cider-switch-to-relevant-repl-buffer nil)
        )

      (defun clojure-insert-println ()
        (interactive)
        ;; (clojure-insert-sexp"(.log js/console \"\")" 2) TODO for cljs
        (clojure-insert-sexp"(println \"\")" 2)
      )

      (defun clojure-insert-let ()
        (interactive)
        ;; (cljr-introduce-let) ; TODO see docu for cljr-introduce-let
        (clojure-insert-sexp "(let [])" 2))

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
        (clojure-insert-sexp "(do)" 1))

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

      (setq cider-cljs-lein-repl ;; set how CIDER starts cljs-lein-repl
            ;; "(do (require 'figwheel-sidecar.repl-api)
            ;;      (figwheel-sidecar.repl-api/start-figwheel!)
            ;;      (figwheel-sidecar.repl-api/cljs-repl))"
            "(do (use 'figwheel-sidecar.repl-api)
                 (start-figwheel!)
                 (cljs-repl))")

      :bind (;; lambdas are not supported
             ("s-x"   . cider-switch-to-repl-buffer)
             ("<s-insert>" . clojure-insert-println)
             ;; (bind-key "C-s-p" 'clojure-insert-println)
             ("C-s-p" . clojure-insert-println)
             ("C-s-l" . clojure-insert-let)
             ("C-s-f" . clojure-insert-for)
             ("C-s-n" . clojure-insert-defn)
             ("C-s-s" . clojure-insert-doseq)
             ("C-s-d" . clojure-insert-do)
             ;; ("s-x"   . cider-switch-to-last-clojure-buffer)
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
             ("s-H"   . helm-cider-history)

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
            (shell-command-on-region (region-beginning)
                                     (region-end) "xsel -i -b")
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
           ;; 4 means: jump to the 2nd slash
           (offset (+ (length search-regex) 4)))
      ;; (cons .. offset) moves the point
      (evil-ex (cons sexp-str offset))))
  (global-set-key (kbd "s-:") 'fabricate-subst-cmd)

  ;; ;; keep the cursor centered to avoid sudden scroll jumps
  ;; (require 'centered-cursor-mode)

  ;; ;; disable in terminal modes
  ;; ;; http://stackoverflow.com/a/6849467/519736
  ;; ;; also disable in Info mode, because it breaks going back with the backspace key
  ;; (define-global-minor-mode my-global-centered-cursor-mode centered-cursor-mode
  ;;   (lambda ()
  ;;     (when (not (memq major-mode
  ;;                      (list 'Info-mode 'term-mode 'eshell-mode 'shell-mode 'erc-mode)))
  ;;       (centered-cursor-mode))))
  ;; (my-global-centered-cursor-mode 1)


  ;; advice, defadvice and letf shouldn't be used:
  ;; https://lists.gnu.org/archive/html/emacs-devel/2012-12/msg00146.html
  ;; Emacs 24.4 replaces this mechanism with advice-add
  ;; advice-add doesn't work well with evil-search-next
  ;; (advice-add 'evil-ex-search-next :before #'spacemacs-buffer//center-line)
  ;; (advice-add 'evil-search-next    :before #'spacemacs-buffer//center-line)
  ;; (advice-add 'evil-search-forward :before #'spacemacs-buffer//center-line)

  ;; (defadvice isearch-update (before my-isearch-update activate)
  ;;   (sit-for 0)
  ;;   (if (and
  ;;        ;; not the scrolling command
  ;;        (not (eq this-command 'isearch-other-control-char))
  ;;        ;; not the empty string
  ;;        (> (length isearch-string) 0)
  ;;        ;; not the first key (to lazy highlight all matches w/o recenter)
  ;;        (> (length isearch-cmds) 2)
  ;;        ;; the point in within the given window boundaries
  ;;        (let ((line (count-screen-lines (point) (window-start))))
  ;;          (or (> line (* (/ (window-height) 4) 3))
  ;;              (< line (* (/ (window-height) 9) 1)))))
  ;;       (let ((recenter-position 0.3))
  ;;         (recenter '(4)))))


  ;; (advice-remove 'magit-stash :after)
  ;; (defun magit-stash-no-msg () (magit-stash ""))
  ;; (advice-add 'magit-stash :after #'magit-stash-no-msg)

  ;; Move by screen lines instead of logical (long) lines
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  ;; Also in visual mode
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-archive-priorities (quote (("melpa-stable" . 1) ("melpa" . 0))))
 '(package-selected-packages (quote ()))
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
