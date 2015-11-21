(defconst emacs-start-time (current-time))
(unless noninteractive
   ;; Emacs is running without interactive terminal
  (message "Loading %s..." load-file-name))

(require 'server)
(unless (server-running-p)
  (server-start))

(setq dotf-dir "~/dev/dotfiles"
      config-dir (concat dotf-dir "/emacs")
      elisp-dir  (concat config-dir "/elisp")
      themes-dir (concat config-dir "/themes")
      message-log-max 16384 ;; max lines to keep in the message log buffer
      inhibit-splash-screen t)

(add-to-list 'custom-theme-load-path
             (concat themes-dir "/zenburn-emacs/"))
(add-to-list 'custom-theme-load-path
             (concat themes-dir "/emacs-color-theme-solarized/"))

;; set bash vars http_proxy/https_proxy/ftp_proxy so
;; url-proxy-services won't be needed
;; TODO use-package https://www.youtube.com/watch?v=2TSKxxYEbII - paredit keys at 27:20
;; TODO M-x describe-personal-keybindings

(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://stable.melpa.org/packages/")
;;              t)

(package-initialize)

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package)
  (package-refresh-contents))

(unless (package-installed-p 'diminish)
  (package-install 'diminish)
  (package-refresh-contents))
(require 'diminish) ;  remove useless strings from mode-line

(unless (package-installed-p 'bind-key)
  (package-install 'bind-key)
  (package-refresh-contents))
(require 'bind-key)

(eval-when-compile ; reduce load time
  (defvar use-package-verbose t) ; measure startup time
  (require 'use-package))

(use-package guide-key :ensure t
  :commands guide-key-mode
  :diminish guide-key-mode
  :init
  ;; All the sequencies do something different
  ;; TODO find out what is guide-key exactly good for
  (setq guide-key/guide-key-sequence
        '("C-x r" "C-x 4" "C-x v" "C-x 8"))
  (guide-key-mode 1))

;; this is a mess: use-package-chords must be called before
;; using keyword :chord
;; (use-package use-package-chords :ensure t
;;   :config (key-chord-mode 1))

;; TODO see lispy the advanced paredit
;; (use-package lispy :ensure t)
;; (use-package eclipse-theme :ensure t)
;; (load-theme 'eclipse t)
;; (require 'eclipse-theme)
;; (disable-theme 'eclipse)  (enable-theme 'eclipse)


(use-package paredit :ensure t
  ;; :disabled t
  :config
  (unbind-key "<C-left>" paredit-mode-map)
  (unbind-key "<C-right>" paredit-mode-map)
  :bind (;; Move the sexp
         ;; ("M-s-<left>"  . paredit-forward-slurp-sexp)
         ;; ("M-s-<right>" . paredit-forward-barf-sexp)
         ;; ("C-s-<left>"  . paredit-backward-barf-sexp)
         ;; ("C-s-<right>" . paredit-backward-slurp-sexp)
         ;; Move the parenthesis
         ("M-s-<left>"  . paredit-forward-barf-sexp)
         ("M-s-<right>" . paredit-forward-slurp-sexp)
         ("C-s-<left>"  . paredit-backward-slurp-sexp)
         ("C-s-<right>" . paredit-backward-barf-sexp))
  :init
  (enable-paredit-mode)
  ;; works only with enabled gui elements: see s-f10
  (use-package paredit-menu :ensure t)
  ;; TODO see sp-forward-sexp sp-backward-sexp, sp-down-sexp, sp-up-sexp
  ;; TODO see sp-next-sexp
  (use-package smartparens :ensure t
    :init
    (require 'smartparens-config)
    (smartparens-global-mode)
    (show-smartparens-mode)))

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

;; TODO cider-startup :defer 3 messes us with s-l
(use-package cider-startup ; :defer 3
  :load-path elisp-dir
  ;; :mode ("\\.clj'\\" . clojure-mode)
  ;; :config (message "cider-startup loaded with :defer 3")
  )

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
  :init (purpose-mode))

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
  :init (global-linum-mode t))

(use-package eshell :defer t :ensure t
  :bind ("s-<f1>" . eshell))

;; reload all buffers when the file is changed
(use-package autorevert :defer t :ensure t
  :init (global-auto-revert-mode t))

;;(desktop-load-default)
;;(desktop-read)

;; (use-package elpy)

;; org-mode is loaded by default - can't be ":defer t"
(use-package org :ensure t
  :config ; executed after require
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
  :init (use-package magit-popup :defer t :ensure t))

;; BUG: environment-lib can't be called in the beginning
(use-package environment-lib :load-path elisp-dir)

(use-package discover :ensure t ; might be useless
  :config (global-discover-mode 1))

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
  ;; Always prefer to load newer files,
  ;; instead of giving precedence to the .elc files
  (setq load-prefer-newer t)

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

(use-package helm-startup :load-path elisp-dir)

(use-package drag-stuff :ensure t
  :config (drag-stuff-global-mode t))

(use-package vimrc-mode :defer t :ensure t
  :config (add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode)))

;; dired is not among *Packages*; can't use :ensure t
(use-package dired :defer t
  :commands dired
  :bind ("s-d" . dired-jump)
  :config
  (setq dired-listing-switches
        "-laGh1v --group-directories-first")

  (bind-keys :map dired-mode-map
             ("s-R" . dired-do-rename))
  :init
  ;; show / hide file details: ( / )
  (use-package dired+ :ensure t)

  (use-package dired-details :ensure t
    ;; try :ensure marmalade if not available on melpa
    :init
    (use-package dired-details+ :ensure t
      :init (use-package dired-details+ :ensure t)))

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
  :init (winner-mode 1))

(use-package smart-mode-line :ensure t :init
  (setq sml/shorten-directory t
        ;; sml/theme 'respectful
        sml/name-width 32
        sml/shorten-modes t
        sml/use-projectile-p 'before-prefixes
        sml/projectile-replacement-format "%s/")
  (add-hook 'after-init-hook 'sml/setup))

(use-package evil-startup :load-path elisp-dir)

;; see also the package buffer-move
(use-package transpose-frame :defer t :ensure t
  :bind (("<f8>"   . transpose-frame)
         ("M-<f8>" . flop-frame)   ; left <-> right
         ("C-<f8>" . flip-frame))) ; top  <-> bottom

;; (use-package ack-menu :defer t :ensure t)

(use-package paradox :defer t :ensure t
  :bind (("<f9>"   . paradox-list-packages) ; TODO auto enable/disable evil-mode
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

(use-package avy :ensure t :defer t
  :bind (("<f2>" . avy-goto-char)
         ("s-j" . avy-goto-char-2)
         ;; avy-goto-word-1
         ("<c-f2>" . avy-goto-line))
  ;;   :init
  ;;   (bind-chords :map global-map
  ;;                ("jj" . ace-jump-char-mode)
  ;;                ("jk" . ace-jump-word-mode)
  ;;                ("jl" . ace-jump-line-mode)
  ;;                ("ss" . ace-jump-mode-pop-mark))
  ;; (when (and (featurep 'evil) (featurep 'evil-leader))
  ;;   (evil-leader/set-key
  ;;     "c" 'ace-jump-char-mode
  ;;     "w" 'ace-jump-word-mode
  ;;     "l" 'ace-jump-line-mode)
  )

(use-package expand-region :defer t :ensure t
  :bind (("C-="        . er/expand-region)
         ("<C-return>" . er/expand-region))
  :init
  (when (and (featurep 'evil) (featurep 'evil-leader))
    (progn
      (setq expand-region-contract-fast-key "z")
      (evil-leader/set-key "xx" 'er/expand-region))))

(use-package yasnippet :defer t :ensure t
  :disabled t
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
          ("github\\.com" . md))
        edit-server-new-frame nil)
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(use-package browse-url :defer t :ensure t
  :init
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program
        ;; "chromium-browser" does not work properly on ubuntu 13.10
        ;; "chrome" ; cygwin
        "google-chrome"))

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

(unless (display-graphic-p)
  (use-package evil-terminal-cursor-changer :defer t :ensure t))

(use-package sublimity :ensure t :defer t
  :init
  ;; sublimity-scroll is loaded together with sublimity - no :ensure needed
  ;; sublimity-scroll does not work if defered
  (use-package smooth-scrolling :ensure t) ; is smooth-scrolling package really needed
  (use-package sublimity-scroll)
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
    (error "Minibuffer not active")))

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

(use-package debbugs :defer t :ensure t) ; emacs bug tracker

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
  (defun untabify-buffer ()
    (interactive)
    (untabify (point-min) (point-max)))

  (defun indent-buffer ()
    (interactive)
    (indent-region (point-min) (point-max)))

  (defun cleanup-buffer ()
    "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
    (interactive)
    (untabify-buffer)
    (delete-trailing-whitespace)
    (indent-buffer))

  (setq require-final-newline t
        show-trailing-whitespace t)
  (set-default 'indicate-empty-lines t))

(use-package popwin :defer t :ensure t ; no annoying buffers
  :config (popwin-mode 1))

;; TODO disable color-identifiers-mode only for specific modes: clojure-mode
(use-package color-identifiers-mode :ensure t :disabled t ; nice but noisy
  :init (add-hook 'after-init-hook 'global-color-identifiers-mode)

  (defun my/enable-color-identifiers ()
    (interactive)
    (color-identifiers-mode t)))

;; visual feedback on some operation
(use-package volatile-highlights :defer t :ensure t
  :config (volatile-highlights-mode t))

(use-package markdown-mode :defer t :ensure t)

;; TODO mmm-mode might be the reason for jumps between a lisp and clojure mode
(use-package mmm-mode :ensure t ; Allow Multiple Major Modes in a buffer
  :config (setq mmm-global-mode 'maybe))

(defcustom smart-to-ascii ; no smart quotes
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
  :init (setq yagist-github-token (getenv "GITHUB_TOKEN")))

(use-package fish-mode :ensure t :defer t) ; fish shell files

;; Elisp go-to-definition with M-. and back again with M-,
;; (autoload 'elisp-slime-nav-mode "elisp-slime-nav")
;; (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t) (eldoc-mode 1)))

(use-package powerline :ensure t
  :config
  (setq powerline-display-buffer-size nil)
  (setq powerline-display-mule-info nil)
  (setq powerline-display-hud nil)
  (when (display-graphic-p)
    (powerline-default-theme))

  :init
  (use-package powerline-evil :ensure t)
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

;; TODO auto-dim-other-buffers should use other font color
;; (use-package auto-dim-other-buffers :defer t :ensure t)

;; Cycle bufffers - see also uzumaki
(use-package cbm :defer t :ensure t ;; Cycle through buffers with the same `major-mode
  :bind ("C-'" . cbm-cycle))

;; (use-package bug-hunter :ensure t)

;; TODO test uniquify
;; (use-package uniquify :ensure t
;;     :init
;;   (setq uniquify-buffer-name-style 'reverse)
;;   (setq uniquify-separator "/")
;;   (setq uniquify-ignore-buffers-re "^\\*"))

;; TODO test emacs-ycmd + ycmd, the code completion system.

(use-package emacs :ensure t
  :bind (("<f10>" . menu-bar-open) ; this is the default
         ("s-e"   . eval-last-sexp)
         ("s-D"   . eval-defun) ; also C-M-x
         ("s-E"   . eval-defun)
         ("s-f"   . eval-defun)
         ("s-F"   . helm-find-files)
         ("s-s"   . save-buffer)
         ("s-x"   . kill-region)    ; cut
         ("s-v"   . yank)           ; paste
         ;; see evil-window-map
         ;; TODO s-q should work like C-tab if only one window is visible
         ("s-q"               . other-window)
         ("s-0"               . delete-window)
         ("s-1"               . delete-other-windows)
         ("s-R"               . rename-file-and-buffer)
         ;; ("<f3>"              . find-grep) ; Use -e '...' instead of -e "..."
         ("<f7>"              . find-file-emacs)
         ("s-k"               . close-buffer)
         ("C-s-k"             . delete-file-and-close-its-buffer)
         ("s-2"               . split-other-window-below)
         ("s-3"               . split-other-window-right)
         ("<s-f10>"           . find-zark-file)
         ("<s-f11>"           . find-emacs-init-file)
         ("<s-f12>"           . switch-to-buffer-scratch)
         ("<C-S-iso-lefttab>" . unbury-buffer)
         ("M-s-h"             . shrink-window-horizontally)
         ("M-s-l"             . enlarge-window-horizontally)
         ("M-s-j"             . enlarge-window)
         ("M-s-k"             . shrink-window)
         ("s-u"               . eval-buffer) ; might be in lisp-mode-keys see ("s-u" . helm-surfraw)
         ("<C-up>"            . xah-backward-block)
         ("<C-down>"          . xah-forward-block)
         ("<C-prior>"         . hs-hide-block)
         ("<C-next>"          . hs-show-block)
         ;; ("<C-M-prior>"       . hs-toggle-hiding)
         ("<C-M-prior>"       . hs-hide-all)
         ("<C-M-next>"        . hs-show-all)

         ;; ("<s-delete>"        . kill-sexp)
         ("<s-backspace>"     . paredit-backward-kill-word)
         ("<s-delete>"        . paredit-forward-kill-word)
         ;; default key binding; transpose current sexp with sexp on the right from cursor
         ;; ("C-M-t"             . transpose-sexp)

         ;; ("<C-kp-multiply>" . highlight-symbol-at-point)
         ;; ("<s-f10>"         . gui-toggle) ;; shows also scrollbars
         ;; ("<s-tab>"         . other-window)

         ;;  ((kbd "C-<f11>") . (lambda ()
         ;;                (interactive)
         ;;                (shell-command
         ;;                 ;; "cvs-ci-hooks.sh"
         ;;                 "")))
         ;;
         ;;  ((kbd "C-<f8>") . (lambda ()
         ;;               (interactive)
         ;;               (shell-command
         ;;                ;; "cvs-test.sh -lo :pserver:faizal@localhost:/myrepos"
         ;;                "")))
         ;;  ((kbd "C-<f12>") . (lambda ()
         ;;                         (interactive)
         ;;                         (shell-command
         ;;                          ;; (concat "cvs-test.sh -fr "
         ;;                          ;;         ":pserver:"
         ;;                          ;;         "rsvoboda@dlnxcvshooksdev01.ptx.fr.sopra"
         ;;                          ;;         ":2401/cvscorfja")
         ;;                          "")))
         ;;
         ;;  ((kbd "<s-f3>") . kmacro-start-macro-or-insert-counter)
         ;;  ((kbd "<s-f4>") . kmacro-end-or-call-macro)
         )
  :config
  ;; (defalias 'qrr 'query-replace-regexp) ; M-x qrr
  (global-prettify-symbols-mode +1)

  (prefer-coding-system 'utf-8)
  (setq backup-inhibited t)

  ;; (setq gui-elements -1)

  :init
  (defun eval-and-replace ()
    "Replace the preceding sexp with its value."
    (interactive)
    (backward-kill-sexp)
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0)))))

  ;; Increase/decrease selective display - indentation
  (defun inc-selective-display (arg)
    (interactive "P")
    (if (numberp arg)
        (set-selective-display arg)
      (if (numberp selective-display)
          (set-selective-display (+ 2 selective-display))
        (set-selective-display 2)))
    (create-temp-selective-display-keymap))

  (defun dec-selective-display ()
    (interactive)
    (when (and (numberp selective-display)
               (> selective-display 2))
      (set-selective-display (- selective-display 2)))
    (create-temp-selective-display-keymap))

  (defun clear-selective-display ()
    (interactive)
    (when (numberp selective-display)
      (set-selective-display nil)))

  (defun create-temp-selective-display-keymap ()
    (set-temporary-overlay-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "+") 'inc-selective-display)
       (define-key map (kbd "-") 'dec-selective-display)
       (define-key map (kbd "0") 'clear-selective-display)
       map))
    (message "Type + to reveal more, - for less, 0 to reset."))

  ;; shorthand for interactive lambdas
  (defmacro interactive-lambda (&rest body)
    `(lambda ()
       (interactive)
       ,@body))

  (defun switch-to-buffer-scratch ()
    (interactive)
    (switch-to-buffer "*scratch*"))

  (defun find-emacs-init-file ()
    (interactive)
    (find-file (concat config-dir "/init.el")))

  (defun find-zark-file ()
    (interactive)
    (find-file (concat elisp-dir "/zark.el")))

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
    ;; :config (grepp-remove-comments)
    )

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

  ;; disable most of this stuff early in the process so it doesn’t flicker.
  ;; (if (fboundp 'tool-bar-mode) (tool-bar-mode gui-elements))
  ;; TODO test fboundp - is it faster?
  ;; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode gui-elements))
  ;; (if (fboundp 'menu-bar-mode) (menu-bar-mode gui-elements))
  ;; (menu-bar-mode gui-elements)
  ;; (scroll-bar-mode gui-elements)

  ;; (defun gui-toggle ()
  ;;   (interactive)
  ;;   (setq gui-elements (* -1 gui-elements))
  ;;   (menu-bar-mode gui-elements)
  ;;   (toggle-scroll-bar gui-elements)
  ;;   (message "gui-elements %s"
  ;;            (if (= 1 gui-elements) "enabled" "disabled")))

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

  (defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
    "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

  (defvar current-time-format "%a %H:%M:%S"
    "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

  (defun timestamp ()
    (interactive)
    (format-time-string current-time-format (current-time)))

  (defun emacs-build-time-formated ()
    (interactive)
    (let ((ebt (format-time-string current-date-time-format
                                   emacs-build-time)))
      (message (concat "emacs-build-time: " ebt))))

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

  (defun emacs-lisp-mode-keys ()
    "Modify keymaps used by `emacs-lisp-mode'."
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
 '(package-selected-packages
   (quote
    (ace-window avy-window avy bug-hunter grep+ cbm powerline-evil powerline fish-mode yagist mmm-mode markdown-mode volatile-highlights popwin smooth-scrolling sublimity minimap duplicate-thing expand-region ace-jump-mode iedit paradox transpose-frame evil-anzu anzu evil-search-highlight-persist evil-leader evil-smartparens evil-numbers evil-surround evil-args evil-nerd-commenter evil-visualstar evil-visual-mark-mode evil smart-mode-line dired-rainbow dired-subtree dired-details+ dired-details dired+ vimrc-mode drag-stuff persp-projectile helm-projectile helm-flycheck google-this helm-google helm-ls-git helm-descbinds macrostep helm-cider-history helm-ag helm-commandlinefu cljr-helm helm neotree discover magit git-timemachine linum-relative company window-purpose clj-refactor rainbow-delimiters ac-cider cider-eval-sexp-fu kibit-helper clojure-mode-extra-font-locking cider slamhound smartparens paredit-menu paredit use-package-chords auto-package-update use-package)))
 '(paradox-github-token t t)
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

(if (boundp 'is-virt-box)
    (use-package color-theme-zenburn :ensure t :defer t
      :init (load-theme 'zenburn 'no-confirm))
  (use-package color-theme-solarized :ensure t :defer t
    :init (load-theme 'solarized 'no-confirm)))

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
