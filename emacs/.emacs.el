;; (setq debug-on-error t) ;; turned off at the end
;; this is for the emacs code browser
;; (setq stack-trace-on-error t)

(load "server")
(unless (server-running-p)
  (server-start))

;; (defconst emacs-start-time (current-time))
;; (unless noninteractive
;;   (message "Loading %s..." load-file-name))

;; max nr of lines to keep in the message log buffer
(setq message-log-max 16384)

(setq dotf-dir "~/dev/dotfiles")
(setq config-dir (concat dotf-dir "/emacs"))
(setq elisp-dir  (concat config-dir "/elisp"))
(setq themes-dir (concat config-dir "/themes"))

(add-to-list 'custom-theme-load-path
             (concat themes-dir "/zenburn-emacs/"))
(add-to-list 'custom-theme-load-path
             (concat themes-dir "/emacs-color-theme-solarized/"))

(setq inhibit-splash-screen t)

;; set bash vars http_proxy/https_proxy/ftp_proxy so
;; url-proxy-services won't be needed
;; TODO use-package https://www.youtube.com/watch?v=2TSKxxYEbII
;; TODO (if (eq system-type 'gnu/linux))
;; TODO do macro expansion for use-package
;; TODO :bind (:map ...-mode-map); :bind is (bin-key)
;; TODO M-x describe-personal-keybindings
;; TODO (or (use-package foo) (use-package bar))
(if (string= system-type "windows-nt")
    (setq url-proxy-services '(("no_proxy" . "work\\.com")
                               ("http" . (getenv "proxy"))
                               ("https" . (getenv "proxy")))))

(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ;; ("org" . "http://orgmode.org/elpa/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("user42" . "http://download.tuxfamily.org/user42/elpa/packages/")))

;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://stable.melpa.org/packages/")
;;              t)

(package-initialize)

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile ; reduce load time
  (require 'use-package))
;; TODO auto install of diminish and bind-key doesn't work
(require 'diminish)
(require 'bind-key)

;; TODO auto-package-update, use-package :ensure dependend on inet availability
;; see :disabled t and :if condition
(use-package auto-package-update :ensure t)

(use-package paredit :defer t :ensure t
  :bind (("s-<left>"  . paredit-backward-slurp-sexp)
         ("s-<right>" . paredit-backward-barf-sexp))
  :init
    ;; works only with enabled gui elements: see s-f10
  (use-package paredit-menu :ensure t))

;;(defun skewer-mode-keys ()
;;  "Modify keymaps used by `skewer-mode'."
;;  (local-set-key (kbd "s-e") 'skewer-eval-last-expression)
;;  (local-set-key (kbd "s-x") 'skewer-eval-defun)
;;  (local-set-key (kbd "s-l") 'skewer-load-buffer)
;;  )
;; skewer works on top of js2-mode
;; (add-hook 'js2-mode-hook 'skewer-mode-keys)
;; (add-hook 'skewer-mode-hook 'skewer-mode-keys)

;; (use-package inf-clojure
;;   :defer t
;;   :ensure t
;;   :init

;;   ;; (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

;;   (defun cljs-node-repl ()
;;     (interactive)
;;     (run-clojure
;;      "java -cp cljs.jar clojure.main repl.clj"
;;      ;; "lein trampoline run -m clojure.main repl.clj"
;;      )))

;; slamhound: rip and reconstruct clojure namespace
(use-package slamhound :defer t :ensure t)
;; TODO cider-startup ":defer t" does not work
(use-package cider-startup
  :load-path elisp-dir)

(use-package clj-refactor :defer t :ensure t
  :init
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (yas-minor-mode 1) ; for adding require/use/import
              ;; eg. rename files with `C-c C-m rf`.
              (cljr-add-keybindings-with-prefix "C-c C-m"))))

;; hide *nrepl-connection* and *nrepl-server* when switching buffers
;; (setq nrepl-hide-special-buffers t)

(use-package window-purpose :defer t :ensure t
  :bind (;; C-c , d: window-purpose
         ("C-s-d" . purpose-toggle-window-purpose-dedicated)
         ;; C-c , D: window-buffer
         ("C-s-D" . purpose-toggle-window-buffer-dedicated))
  :init
  (purpose-mode))

;; (use-package auto-complete-config :disabled t
;;   :init
;;   (ac-config-default)
;;   (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;   (use-package ac-cider
;;     :init
;;     (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
;;     (add-hook 'cider-mode-hook 'ac-cider-setup)
;;     (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
;;     (eval-after-load "auto-complete"
;;       '(progn
;;          (add-to-list 'ac-modes 'cider-mode)
;;          (add-to-list 'ac-modes 'cider-repl-mode))))
;; ;; TODO compare auto-complete and company-mode (supported by cider):
;; https://github.com/company-mode/company-mode/issues/68
(use-package company :ensure t :defer t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package linum-relative :defer t :ensure t
  :bind ("C-s-n" . linum-relative-toggle)
  :init
  (global-linum-mode t))

(use-package autorevert :defer t :ensure t
  :init
  ;; reload all buffers when the file is changed
  (global-auto-revert-mode t))

;;(desktop-load-default)
;;(desktop-read)

;; (use-package elpy)

(use-package jedi :ensure t :defer t
  :init
  (use-package company-jedi :ensure t :defer t
    :init
    (defun my/python-mode-hook ()
      (add-to-list 'company-backends 'company-jedi))
    (add-hook 'python-mode-hook 'my/python-mode-hook)))

;; org-mode is loaded by default - can't be ":defer t"
(use-package org :ensure t
  :config
  ;; (use-package org-install)
  ;; (org-babel-do-load-languages
  ;; 'org-babel-load-languages
  ;; '(
  ;;   (emacs-lisp . t)
  ;;   (clojure . t)
  ;;   (sh . t)
  ;;   (python .t)
  ;;   (R . t)
  ;;   (ruby . t)
  ;;   (ditaa . t)
  ;;   (dot . t)
  ;;   (sqlite . t)
  ;;   (perl . t)
  ;;   ))
  ;; Show syntax highlighting per language native mode in *.org
  (setq org-src-fontify-natively t)
  ;; For languages with significant whitespace like Python:
  (setq org-src-preserve-indentation t)

  ;; Add shortcuts for ogr-agenda
  ;;(global-set-key "\C-cl" 'org-store-link)
  ;;(global-set-key "\C-cc" 'org-capture)
  ;;(global-set-key "\C-ca" 'org-agenda)
  )


;; Setup custom shortcuts
;;(global-set-key "\C-x\C-g" 'goto-line)
;;(global-set-key [f1] 'compile)
;;(global-set-key [f2] 'next-error)

;; (global-set-key [f6] 'split-window-horizontally)
(use-package git-timemachine :ensure t :defer t)

(use-package magit :defer t :ensure t
  :bind ("s-m" . magit-status)
  :init
  (use-package magit-popup :defer t :ensure t)
  (setq magit-auto-revert-mode t)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (autoload 'magit-status "magit" nil t))

(use-package environment-lib
  :load-path elisp-dir)

;; -t: semicolon is the command line terminator.
;; default is end-of-line as a SQL statement terminator
;; (setq sql-db2-options '("-c" "-i" "-w" "db2" "-tv"))

;; simppe is probably the base package emacs; can't use :ensure t
(use-package simple
 :bind (;; ("<S-delete>". kill-line)
        ("<S-delete>" . kill-region)
        ;; clipboard-kill-region belongs to package menu-bar
        ;; ("<S-delete>" . clipboard-kill-region)

        ;; C-s-backspace is the default key binding for kill-whole-line
        ("<C-s-backspace>" . kill-line-backward)
        ("<C-S-delete>"    . kill-line)
        ;; (define-key global-map [f5] 'toggle-truncate-lines)
        )
  :init
  (defun buffer-mode (buffer-or-string)
    "Returns the major mode associated with a buffer."
    (with-current-buffer buffer-or-string
      major-mode))

  (defun this-buffer-mode ()
    "Returns the major mode associated with current buffer."
    (interactive)
    (message (concat "Major mode: "
                     (prin1-to-string (buffer-mode (buffer-name))))))

  ;; prevent: Error saving to X clipboard manager.
  (setq x-select-enable-clipboard-manager nil)

  (set-face-attribute 'default nil :height (get-font-height))
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; minibuffer completion incremental feedback
  (icomplete-mode 99)  ; turn on icomplete-mode

  ;; pretty syntax highlighting everywhere
  (global-font-lock-mode t)

  (size-indication-mode 1)  ; filesize indicator
  (setq truncate-lines t) ;; no line wrap
  (column-number-mode 1)

  (defun kill-line-backward (arg)
    "Kill ARG lines backward."
    (interactive "p")
    (kill-line (- 1 arg))))

(use-package neotree :defer t :ensure t
  :bind ("<s-f8>" . neotree-toggle))

;; icicles - Minibuf input completion & cycling of completion candidates
;; (use-package icicles :ensure t  :defer t)

(use-package helm-startup
  :load-path elisp-dir)

(use-package drag-stuff :defer t :ensure t
  :init
  (drag-stuff-global-mode t))

(use-package vimrc-mode :defer t :ensure t
  :init
  (require 'vimrc-mode)
  (add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode)))

;; dired is not among *Packages*; can't use :ensure t
(use-package dired :defer t
  :bind ("s-d" . dired-jump)
  :config
  (bind-keys :map dired-mode-map
             ("s-r" . dired-do-rename))
  :init
  (use-package dired-details :defer t :ensure t
    ;; try :ensure marmalade if not available on melpa
    :init
    (require 'dired-details)
    (dired-details-install))

  (use-package dired-details+ :defer t :ensure t
   :init
   (use-package dired+ :defer t :ensure t
     ;; show / hide file details: ( / )
    :init
    (require 'dired+))
   (require 'dired-details+))

  (use-package dired-subtree :defer t :ensure t
    :config
    (bind-keys :map dired-mode-map
            ("<C-return>"   . dired-subtree-insert)
            ("<C-M-return>" . dired-subtree-remove)))

  ;; file highlighting
  (use-package dired-rainbow :defer t :ensure t)
  ;; When moving to parent directory by `^´, Dired by default creates a
  ;; new buffer for each movement up. The following rebinds `^´ to use
  ;; the same buffer
  ;; (add-hook 'dired-mode-hook
  ;;  (lambda ()
  ;;   (define-key dired-mode-map (kbd "^")
  ;;     (lambda () (interactive) (find-alternate-file "..")))
  ;;   ; was dired-up-directory
  ;;  ))
  ;;(toggle-diredp-find-file-reuse-dir 1)
  ;; (diredp-find-file-reuse-dir-buffer 1)
  ;; (diredp-mouse-find-file-reuse-dir-buffer [mouse-1])
  ;; (diredp-subst-find-alternate-for-find)
  (defun kill-dired-buffers ()
    (interactive)
    (mapc (lambda (buffer)
            (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
              (kill-buffer buffer)))
          (buffer-list))))

;; layout management
(use-package winner :defer t :ensure t
  :init
  (winner-mode 1))

(use-package smart-mode-line :ensure t :init
  (setq sml/shorten-directory t
        ;; sml/theme 'respectful
        sml/name-width 32
        sml/shorten-modes t
        sml/use-projectile-p 'before-prefixes
        sml/projectile-replacement-format "%s/")
  (add-hook 'after-init-hook 'sml/setup))

(use-package evil-startup
  :load-path elisp-dir)

;; see package buffer-move
(use-package transpose-frame :defer t :ensure t
  :load-path elisp-dir
  :bind (("<f8>"   . transpose-frame)
         ("M-<f8>" . flop-frame))
  :init
  ;; TODO check how to automate byte-compilation of transpose-frame
  ;; it's neccessary to require transpose-frame -  otherwise undef
  (require 'transpose-frame))

(use-package time :ensure t
  :init
  (setq display-time-24hr-format 1)
  (display-time-mode 1))

(defun back-window ()
  ;; opposite of other-window
  (interactive)
  (other-window -1))

(use-package paradox :defer t :ensure t
  :bind (;; ("f9" . paradox-list-packages) ; TODO auto enable/disable evil-mode
         ("<s-f9>" . paradox-upgrade-packages))
  :init
  (defun package-auto-upgrade ()
    (interactive)
    (package-list-packages)
    (package-menu-mark-obsolete-for-deletion)
    (package-menu-mark-upgrades)
    (package-menu-execute))

  (use-package spinner :defer t :ensure t)
  (setq paradox-github-token (getenv "GITHUB_TOKEN")
        paradox-automatically-star t))

;; (setq inferior-lisp-program "browser-repl")
;; (setq inferior-lisp-program "cljs-repl")
;; (message (concat "inferior-lisp-program: " inferior-lisp-program))

;; (setq inferior-lisp-buffer "browser-repl")
;; (message inferior-lisp-buffer)

;; edit every instance of word/variable in the buffer - like multiple cursors
(use-package iedit :defer t :ensure t
  :bind ("s-i" . iedit-mode))

(use-package multiple-cursors :defer t :ensure t
  :bind (("C->" . mc/mark-all-like-this-in-defun)
         ;; ("C->" .  mc/mark-next-like-this)
         ;; ("C-M->" . mc/unmark-next-like-this)

         ("C-<" . mc/mark-previous-like-this)
         ("C-M-<" . mc/unmark-previous-like-this)

         ;; ("C->" . mc/mark-next-word-like-this)
         ;; ("C-M->" . mc/unmark-next-word-like-this)
         ;; ("C-<" . mc/mark-previous-word-like-this)
         ;; ("C-M-<" . mc/unmark-previous-word-like-this)

         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c C-M-<" . mc/unmark-all-like-this)

         ;; ("C-S-c C-S-c" . mc/edit-lines)
         ;; ("C->" . mc/mark-next-like-this)
         ;; ("C-<" . mc/mark-previous-like-this)
         ;; ("C-c C-<" . mc/mark-all-like-this)
         ))

(use-package ace-jump-mode :ensure t :defer t
  :bind (("<f2>" . ace-jump-mode)
         ("s-j"  . ace-jump-mode)
         ;; ("s-a" . ace-jump-buffer) ; see helm-buffers-list
         ("<C-f2>" . ace-jump-line-mode))
  :init
  (autoload 'ace-jump-mode "ace-jump-mode" nil t)

  (when (and (featurep 'evil) (featurep 'evil-leader))
    (evil-leader/set-key
      "c" 'ace-jump-char-mode
      "w" 'ace-jump-word-mode
      "l" 'ace-jump-line-mode)))

(use-package expand-region :defer t :ensure t
  :bind ("C-=" . er/expand-region)
  :init
  (when (and (featurep 'evil) (featurep 'evil-leader))
    (progn
      (setq expand-region-contract-fast-key "z")
      (evil-leader/set-key "xx" 'er/expand-region))))

(use-package yasnippet :defer t :ensure t
  :init
  (let ((yasnippet-dir "~/.emacs.d/plugins/yasnippet"))
    (shell-command-to-string (concat "mkdir -p " yasnippet-dir))
    (add-to-list 'load-path yasnippet-dir))
  (yas-global-mode 1)
  ;; (define-key yas-minor-mode-map (kbd "s-y") 'yas/expand)
  ;; (define-key yas-minor-mode-map (kbd "TAB") nil)
  )

;; Repond to requests from the Emacs Chrome plugin using sockets.
;; See emacs activation docu in the browser plugin
(use-package edit-server :disabled t :defer t
  :if window-system
  :init
  (add-to-list 'load-path "~/.emacs.d")
  (setq edit-server-url-major-mode-alist
        '(("github\\.com" . markdown-mode)
          ("github\\.com" . md)))
  (setq edit-server-new-frame nil)
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(use-package browse-url :defer t :ensure t
  :init
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program
        ;; "chromium-browser" does not work properly on ubuntu 13.10
        ;; "chrome" ; cygwin
        "google-chrome"))

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; xfce4-settings-manager -> Window Manger -> Keyboard -> ...
(use-package duplicate-thing :defer t :ensure t
  :bind (("C-M-<up>"   . duplicate-thing)
         ("C-M-<down>" . duplicate-thing)))

;; (defun ignore-error-wrapper (fn)
;;   "Funtion return new function that ignore errors.
;;    The function wraps a function with `ignore-errors' macro."
;;   (lexical-let ((fn fn))
;;     (lambda ()
;;       (interactive)
;;       (ignore-errors
;;         (funcall fn)))))

(use-package minimap :defer t :ensure t
  :bind ("C-c C-m" . minimap-toggle))

;; (load-library "abbrev-table")
;;(global-set-key [f11] 'abbrev-mode)
;;(global-set-key [f11] 'toggle-frame-fullscreen) ; this is the default
(toggle-frame-maximized)

(use-package undo-tree :ensure t
  :defer (2 global-undo-tree-mode t) ; load after 2 seconds of idle time
  :diminish ""
  :bind (("C-x u" . undo-tree-visualize) ;; default
         ("<f12>" . undo-tree-visualize)
         ("C-/"   . undo-tree-undo)))

;; (global-set-key [scroll] 'exec-test-macro)

(defun switch-to-buffer-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(unless (display-graphic-p)
  (use-package evil-terminal-cursor-changer :defer t :ensure t))

;; (define-key global-map [(control ?z) ?u] 'uniq-lines)

(use-package ace-window :defer t :ensure t
  :bind ("M-o" . ace-window)
  :init
  ;; the sequence of leading characters for each window:
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package sublimity :ensure t
  :init
  ;; only smooth-scrolling together with sublimity leads to
  ;; smooth scrolling really working! WTF?
  (use-package smooth-scrolling :ensure t)
  (use-package sublimity-scroll); inside sublimity :ensure t not needed
  (sublimity-mode 1))

;; another possibility how to define a key chord:
;; (global-set-key [(control x) (?0)] 'sticky-window-delete-window)
(use-package sticky-windows
  ;; sticky-windows must by downloaded from
  ;; http://www.emacswiki.org/emacs/download/sticky-windows.el
  ;; :ensure t
  :bind (("C-x 0" . sticky-window-delete-window)
         ("C-x 1" . sticky-window-delete-other-windows)
         ("C-x 9" . sticky-window-keep-window-visible)))

(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(global-set-key "\C-co" 'switch-to-minibuffer) ;; Bind to: C-c o

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)


;; use following when: Source file '...' newer than byte-compiled file
;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;; (defun display-code-line-counts (ov)
;;   (when (eq 'code (overlay-get ov 'hs))
;;     (overlay-put ov 'help-echo
;;                  (buffer-substring (overlay-start ov)
;;                                    (overlay-end ov)))))

;; (setq hs-set-up-overlay 'display-code-line-counts)

(use-package whitespace :defer t :ensure t
  :diminish whitespace-mode
  :bind (("s-w"    . whitespace-mode)
         ("<s-f7>" . whitespace-cleanup)
         ;; ((kbd "s-<f7>") . (lambda ()
         ;;                           (whitespace-cleanup)
         ;;                           (interactive)
         ;;                           (message "whitespace-cleanup done.")))
         )
  :init
  (setq require-final-newline t)
  (set-default 'indicate-empty-lines t)
  (setq show-trailing-whitespace t))

;; Always prefer to load newer files,
;; instead of giving precedence to the .elc files
(setq load-prefer-newer t)

(use-package popwin :defer t :ensure t
  :init
  (require 'popwin)
  (popwin-mode 1)


  (defvar popwin:special-display-config-backup popwin:special-display-config)
  ;; (setq display-buffer-function 'popwin:display-buffer)

  ;; basic
  (push '("*Help*" :stick t :noselect t) popwin:special-display-config)
  (push '("*helm world time*" :stick t :noselect t)
        popwin:special-display-config)

  ;; magit
  (push '("*magit-process*" :stick t) popwin:special-display-config)

  ;; quickrun
  (push '("*quickrun*" :stick t) popwin:special-display-config)

  ;; dictionaly
  (push '("*dict*" :stick t) popwin:special-display-config)
  (push '("*sdic*" :stick t) popwin:special-display-config)

  ;; popwin for slime
  (push '(slime-repl-mode :stick t) popwin:special-display-config)

  ;; man
  (push '(Man-mode :stick t :height 20) popwin:special-display-config)

  ;; Elisp
  (push '("*ielm*" :stick t) popwin:special-display-config)
  (push '("*eshell pop*" :stick t) popwin:special-display-config)

  ;; pry
  (push '(inf-ruby-mode :stick t :height 20) popwin:special-display-config)

  ;; python
  (push '("*Python*"   :stick t) popwin:special-display-config)
  (push '("*Python Help*" :stick t :height 20) popwin:special-display-config)
  (push '("*jedi:doc*" :stick t :noselect t) popwin:special-display-config)

  ;; Haskell
  (push '("*haskell*" :stick t) popwin:special-display-config)
  (push '("*GHC Info*") popwin:special-display-config)

  ;; sgit
  (push '("*sgit*" :position right :width 0.5 :stick t)
        popwin:special-display-config)

  ;; git-gutter
  (push '("*git-gutter:diff*" :width 0.5 :stick t)
        popwin:special-display-config)

  ;; direx - simple directory browser
  (push '(direx:direx-mode :position left :width 40 :dedicated t)
        popwin:special-display-config)

  (push '("*Occur*" :stick t) popwin:special-display-config)

  ;; prodigy - manage external services from emacs
  (push '("*prodigy*" :stick t) popwin:special-display-config)

  ;; malabar-mode - better java mode
  (push '("*Malabar Compilation*" :stick t :height 30)
        popwin:special-display-config)

  ;; org-mode
  (push '("*Org tags*" :stick t :height 30)
        popwin:special-display-config)

  ;; Completions
  (push '("*Completions*" :stick t :noselect t) popwin:special-display-config)

  ;; ggtags
  (push '("*ggtags-global*" :stick t :noselect t :height 30)
        popwin:special-display-config)

  ;; async shell commands
  (push '("*Async Shell Command*" :stick t) popwin:special-display-config))

;; TODO disable color-identifiers-mode only for specific modes: clojure-mode
(use-package color-identifiers-mode :ensure t
  :disabled t ; color-identifiers-mode is nice but noisy
  :init
  (add-hook 'after-init-hook 'global-color-identifiers-mode)

  (defun my/enable-color-identifiers ()
    (interactive)
    (color-identifiers-mode t)))

(use-package volatile-highlights :ensure t
  :config
  (volatile-highlights-mode t))

(use-package markdown-mode :ensure t
  :init
  (add-hook 'markdown-mode-hook
            (lambda () (electric-indent-local-mode -1))))

(use-package mmm-mode :ensure t
  :config
  (setq mmm-global-mode 'maybe))

;; I hate smart quotes, too
(defcustom smart-to-ascii
  '(("\x201C" . "\"")
    ("\x201D" . "\"")
    ("\x2018" . "'")
    ("\x2019" . "'")
    ;; en-dash
    ("\x2013" . "-")
    ;; em-dash
    ("\x2014" . "-"))
  "Map of smart quotes to their replacements"
  :type
  '(repeat (cons (string :tag "Smart Character  ")
                 (string :tag "Ascii Replacement"))))

(defun my/smart-to-ascii (beg end)
  "Replace smart quotes and dashes with their ASCII equivalents"
  (interactive "r")
  (format-replace-strings smart-to-ascii nil beg end))

;; TODO yagist RSA fingerprint
;; # The authenticity of host 'gist.github.com (192.30.252.141)' can't be established.
;; # RSA key fingerprint is ...
;; # Are you sure you want to continue connecting (yes/no)? yes
(use-package yagist :ensure t :defer t
  :init
  (setq yagist-github-token (getenv "GITHUB_TOKEN")))

(use-package fish-mode :ensure t :defer t)

(use-package powerline :defer t :ensure t
  :init
  (use-package powerline-evil :defer t :ensure t
    ;; :init (powerline-evil-center-color-theme)
    )
  (defvar mode-line-cleaner-alist
    `((auto-complete-mode . " α")
      (yas-minor-mode . " γ")
      (paredit-mode . " Φ")
      (eldoc-mode . "")
      (abbrev-mode . "")
      (undo-tree-mode . " τ")
      (volatile-highlights-mode . " υ")
      (elisp-slime-nav-mode . " δ")
      (nrepl-mode . " ηζ")
      (nrepl-interaction-mode . " ηζ")
      ;; Major modes
      (clojure-mode . "λ")
      (hi-lock-mode . "")
      (python-mode . "Py")
      (emacs-lisp-mode . "EL")
      (markdown-mode . "md"))
    "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

  (defun clean-mode-line ()
    (interactive)
    (loop for cleaner in mode-line-cleaner-alist
          do (let* ((mode (car cleaner))
                    (mode-str (cdr cleaner))
                    (old-mode-str (cdr (assq mode minor-mode-alist))))
               (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
               (when (eq mode major-mode)
                 (setq mode-name mode-str)))))

  (add-hook 'after-change-major-mode-hook 'clean-mode-line)
;;; Greek letters - C-u C-\ greek ;; C-\ to revert to default
;;; ς ε ρ τ υ θ ι ο π α σ δ φ γ η ξ κ λ ζ χ ψ ω β ν μ
  )

(use-package emacs :ensure t
  :bind
  (
  ("<f10>" . menu-bar-open) ; this is the default
  ("s-s"   . save-buffer)
  ("s-f"   . find-file)
  ("s-c"   . kill-ring-save) ; copy
  ("s-x"   . kill-region)    ; cut
  ("s-v"   . yank)           ; paste
  ;; see evil-window-map
  ;; TODO s-q should work like C-tab if only one window is visible
  ("s-q"               . other-window)
  ("s-0"               . delete-window)
  ("s-1"               . delete-other-windows)
  ("C-s-r"             . rename-file-and-buffer)
  ("<f3>"              . find-grep) ; Use -e '...' instead of -e "..."
  ("<f7>"              . find-file-emacs)
  ("s-k"               . close-buffer)
  ("C-s-k"             . delete-file-and-close-its-buffer)
  ("s-2"               . split-other-window-below)
  ("s-3"               . split-other-window-right)
  ("<s-f11>"           . find-emacs-init-file)
  ("<s-f12>"           . switch-to-buffer-scratch)
  ("<C-tab>"           . bury-buffer)
  ("<C-S-iso-lefttab>" . unbury-buffer)
  ("C-`"               . unbury-buffer)
  ("M-s-<left>"        . shrink-window-horizontally)
  ("M-s-<right>"       . enlarge-window-horizontally)
  ("M-s-<down>"        . enlarge-window)
  ("M-s-<up>"          . shrink-window)
  ("s-u"               . eval-buffer) ; might be in lisp-mode-keys see ("s-u" . helm-surfraw)


  ;; (bind-key (kbd "<C-kp-multiply>") 'highlight-symbol-at-point)
  ;; (bind-key (kbd "<s-f10>") 'gui-toggle) ;; shows also scrollbars
  ;; (bind-key (kbd "<s-tab>") 'other-window)

;  ((kbd "C-<f11>") . (lambda ()
;                (interactive)
;                (shell-command
;                 ;; "cvs-ci-hooks.sh"
;                 "")))
;
;  ((kbd "C-<f8>") . (lambda ()
;               (interactive)
;               (shell-command
;                ;; "cvs-test.sh -lo :pserver:faizal@localhost:/myrepos"
;                "")))
;  ((kbd "C-<f12>") . (lambda ()
;                         (interactive)
;                         (shell-command
;                          ;; (concat "cvs-test.sh -fr "
;                          ;;         ":pserver:"
;                          ;;         "rsvoboda@dlnxcvshooksdev01.ptx.fr.sopra"
;                          ;;         ":2401/cvscorfja")
;                          "")))
;
;  ((kbd "<s-f3>") . kmacro-start-macro-or-insert-counter)
;  ((kbd "<s-f4>") . kmacro-end-or-call-macro)
   )
  :init
  (defun find-emacs-init-file ()
    (interactive)
    (find-file (concat config-dir ".emacs.el")))

  (defun split-other-window-and (f)
    (funcall f)
    (other-window 1))

  (defun split-other-window-below ()
    (interactive)
    (split-other-window-and 'split-window-below))

  (defun split-other-window-right ()
    (interactive)
    (split-other-window-and 'split-window-right))

  ;; (bind-key [C-s-left] (ignore-error-wrapper 'windmove-left))
  ;; (bind-key [C-s-right] (ignore-error-wrapper 'windmove-right))
  ;; (bind-key [C-s-up] (ignore-error-wrapper 'windmove-up))
  ;; (bind-key [C-s-down] (ignore-error-wrapper 'windmove-down))

  ;; (bind-key "s-b" 'ido-switch-buffer) ; s-b used for helm-mini
  ;; cycle through buffers with Ctrl-Tab / Shift-Ctrl-Tab

  (use-package grep+ :defer t :ensure t
    :init
    (require 'grep+))

  ;; (grep-apply-setting
  ;;  'grep-find-command
  ;;  (concat "find ~/dev/cvs-1.12.13+real"
  ;;          " -type f"
  ;;          " ! -name '*~' -and ! -name '*.o' ! -name 'config.log'"
  ;;          " -and ! -name 'ChangeLog*' -exec grep -nH -e '' {} +"))

  ;; C-h clashes with "help"
  ;; (bind-key (kbd "C-h") 'find-grep) ; as in eclipse

  (defun rename-file-and-buffer ()
    "Rename the current buffer and file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
          (message "Buffer is not visiting a file!")
        (let ((new-name (read-file-name "New name: " filename)))
          (cond
           ((vc-backend filename) (vc-rename-file filename new-name))
           (t
            (rename-file filename new-name t)
            (set-visited-file-name new-name t t)))))))

  (setq gui-elements -1)

  ;; disable most of this stuff early in the process so it doesn’t flicker.
  ;; (if (fboundp 'tool-bar-mode) (tool-bar-mode gui-elements))
  ;; TODO test fboundp - is it faster?
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode gui-elements))
  (if (fboundp 'menu-bar-mode) (menu-bar-mode gui-elements))
  ;; (menu-bar-mode gui-elements)
  ;; (scroll-bar-mode gui-elements)

  (defun gui-toggle ()
    (interactive)
    (setq gui-elements (* -1 gui-elements))
    (menu-bar-mode gui-elements)
    (toggle-scroll-bar gui-elements)
    (message "gui-elements %s"
             (if (= 1 gui-elements) "enabled" "disabled")))


  (defun xah-forward-block (&optional φn)
    "Move cursor forward to the beginning of next text block.
A text block is separated by blank lines. In most major modes,
this is similar to `forward-paragraph', but this command's
behavior is the same regardless of syntax table."
    (interactive "p")
    (search-forward-regexp "\n[\t\n ]*\n+" nil "NOERROR" φn))

  (defun xah-backward-block (&optional φn)
    "Move cursor backward to previous text block.
See: `xah-forward-block'"
    (interactive "p")
    (dotimes (ξn φn) (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR")
                         (progn
                           (skip-chars-backward "\n\t "))
                       (progn (goto-char (point-min))))))

  (bind-key (kbd "<C-up>") 'xah-backward-block)
  (bind-key (kbd "<C-down>") 'xah-forward-block)

  (defun timestamp ()
    "Use bash function 'timestamp' defined in bash/aliases"
    (interactive)
    (insert (shell-command-to-string "timestamp")))

  (define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)

  (defun save-macro (name)
    "Save the last defined macro under 'name' at the end of .emacs"
    (interactive "SName of the macro :")  ; ask for the name of the macro
    (kmacro-name-last-macro name)         ; use this name for the macro
    (find-file (concat config-dir ".emacs.el")) ; open .emacs/other user init file
    (goto-char (point-max))               ; go to the end of the .emacs
    (newline)                             ; insert a newline
    (insert-kbd-macro name)               ; copy the macro
    (newline)                             ; insert a newline
    (switch-to-buffer nil))               ; return to the initial buffer

  ;; (global-set-key (kbd "M-s") 'save-buffer)
  ;; s-s is here just to have consistent key mapping.
  ;; If it's gonna work I can use M-s for something else
  (defun close-buffer ()
    (interactive)
    (if server-buffer-clients
        (server-edit)
      (kill-this-buffer)))
  (defun delete-file-and-close-its-buffer ()
    (interactive)
    (let ((file-name (buffer-file-name (current-buffer))))
      (delete-file file-name)
      (message (concat "File deleted: " file-name)))
    (close-buffer))

  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(csv-separators (quote (";")))
   '(ecb-options-version "2.40")
   '(ecb-source-path (quote ("~/dev/webcli")))
   '(evil-search-highlight-persist t)
   '(git-commit-summary-max-length 70)
   '(global-evil-search-highlight-persist t)
   '(global-hl-line-mode t)
   '(indent-tabs-mode nil)
   '(show-paren-mode t)
   '(tab-width 4)
   '(tool-bar-mode nil nil (tool-bar)))

  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  ;; test delimiters:
  ;; (((( ((( () ))) )))) [[[[ [[[ [] ]]] ]]]] {{{{ {{{ {} }}} }}}}

  ;; check on saving if the file contains a shebang; if yes make it executable
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(evil-search-highlight-persist-highlight-face
     ((t (:background "dark olive green" :foreground "white"))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "dark goldenrod"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "goldenrod"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "light goldenrod"))))
   '(region ((t (:background "#006400")))))

  (add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

  (defun emacs-lisp-mode-keys ()
    "Modify keymaps used by `emacs-lisp-mode'."
    (local-set-key (kbd "s-e") 'eval-last-sexp)
    (if (featurep 'evil-leader)
        (evil-leader/set-key "e" 'eval-last-sexp)))
  (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-keys)

  ;; store / restore : C-x r j / C-x r j w
  ;; (window-configuration-to-register ?w)

  ;; (setq tramp-default-method "ssh")
  )

;; (use-package workgroups2
;;   :defer t
;;   :disabled t ; workgroups2 is broken - it screws minibuffer
;;   :ensure t
;;   :init
;;   ;; must be called 'at the end of .emacs'
;;   (workgroups-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-cljs-repl "(cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env))")
 '(csv-separators (quote (";")))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote ("~/dev/webcli")))
 '(evil-search-highlight-persist t t)
 '(frame-background-mode (quote dark))
 '(git-commit-summary-max-length 70)
 '(global-evil-search-highlight-persist t)
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
 '(paradox-github-token t)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil nil (tool-bar))
 '(vc-follow-symlinks nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-search-highlight-persist-highlight-face ((t (:background "dark olive green" :foreground "white"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark goldenrod"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "goldenrod"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "light goldenrod"))))
 '(region ((t (:background "#006400")))))

;; (load-theme 'zenburn t)
;; (disable-theme 'zenburn)  (enable-theme 'zenburn)

(load-theme 'solarized t)
;; (disable-theme 'solarized)  (enable-theme 'solarized)

;; (setq debug-on-error nil)
