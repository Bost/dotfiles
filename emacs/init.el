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
      message-log-max 16384 ;; max lines to keep in the message log buffer
      inhibit-splash-screen t)

;; set bash vars http_proxy/https_proxy/ftp_proxy so
;; url-proxy-services won't be needed
;; TODO use-package https://www.youtube.com/watch?v=2TSKxxYEbII - paredit keys at 27:20
;; TODO M-x describe-personal-keybindings

(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("org" . "http://orgmode.org/elpa/")
                         ;; '("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

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

;; (use-package server ; TODO analyse 'use-package server'
;;   :if window-system
;;   :init (add-hook 'after-init-hook 'server-start t))

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

(use-package slamhound :defer t :ensure t) ;; rip'n'reconstruct clj namespace

;; TODO cider-startup :defer 3 messes us with s-l
(use-package cider-startup ; :defer 3
  :load-path elisp-dir
  ;; :mode ("\\.clj'\\" . clojure-mode)
  ;; :config (message "cider-startup loaded with :defer 3")
  )

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
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package linum-relative :defer t :ensure t
  :bind ("C-s-n" . linum-relative-toggle)
  :init (global-linum-mode t))

(use-package eshell :defer t :ensure t
  :bind ("s-<f1>" . eshell))

;; reload all buffers when the file is changed
(use-package autorevert :defer t :ensure t
  :init (global-auto-revert-mode t))

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
  :diminish "⟺"
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

  (use-package rainbow-delimiters :defer t :ensure t
    :init
    (if nil '(((((((((((((((((())))))))))))))))))) ; testing ground
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    ;; (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
    ;; (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
    ;; (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
    ;; (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

    (custom-set-faces
     ;; '(rainbow-delimiters-depth-1-face ((t (:foreground "dark goldenrod"))))
     ;; '(rainbow-delimiters-depth-2-face ((t (:foreground "goldenrod"))))
     ;; '(rainbow-delimiters-depth-3-face ((t (:foreground "light goldenrod"))))

     ;; '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
     ;; '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
     ;; '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
     ;; '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
     ;; '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
     ;; '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
     ;; '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
     ;; '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1"))))
     ))

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

(use-package winner :defer t :ensure t ; window layout management
  :init (winner-mode 1))

(use-package smart-mode-line :ensure t
  :disabled t ; default stuff seems to be Ok
  :config
  ;; (setq sml/name-width 32
  ;;       sml/use-projectile-p 'before-prefixes
  ;;       sml/projectile-replacement-format "%s/")
  (setq sml/no-confirm-load-theme t)    ; workaround; (custom-set-variables ...)
                                        ; should be at the very top of init.el
  (sml/setup))

;; git-gutter / git-gutter-fringe - +/- signs for changes lines
(use-package git-gutter-fringe :ensure t
  :disabled t
  :config (progn (set-face-foreground 'git-gutter-fr:added    "green4")
                 (set-face-foreground 'git-gutter-fr:modified "grey50")
                 (set-face-foreground 'git-gutter-fr:deleted  "red3")
                 (fringe-helper-define 'git-gutter-fr:added nil
                   "........"
                   "...XX..."
                   "...XX..."
                   "XXXXXXXX"
                   "XXXXXXXX"
                   "...XX..."
                   "...XX..."
                   "........")
                 (fringe-helper-define 'git-gutter-fr:modified nil
                   "........"
                   "XXXXX..."
                   "XXXXX..."
                   "XXXXX..."
                   "XXXXX..."
                   "XXXXX..."
                   "XXXXX..."
                   "........")
                 (global-git-gutter-mode))
  :init (use-package git-gutter :ensure t :diminish ""))

(use-package evil-startup :load-path elisp-dir)

(use-package buffer-move :ensure t ; see also tramspose-frame
  :bind (("<C-s-kp-up>"    . buf-move-up)
         ("<C-s-kp-8>"     . buf-move-up)
         ("<C-s-kp-down>"  . buf-move-down)
         ("<C-s-kp-2>"     . buf-move-down)
         ("<C-s-kp-left>"  . buf-move-left)
         ("<C-s-kp-4>"     . buf-move-left)
         ("<C-s-kp-right>" . buf-move-right)
         ("<C-s-kp-6>"     . buf-move-right)))

(use-package transpose-frame :defer t :ensure t ; see also buffer-move
  :bind (("<f8>"   . transpose-frame)
         ("M-<f8>" . flop-frame) ; left <-> right
         ("C-<f8>" . flip-frame) ; top  <-> bottom
         ("s-("    . rotate-frame-anticlockwise)
         ("s-)"    . rotate-frame-clockwise)))

;; (use-package ack-menu :defer t :ensure t)

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
  :bind (("<f2>" . avy-goto-word-1)
         ;; Autoloading failed to define function avy-goto-char-1
         ("s-j" . avy-goto-char-1)
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
         ("<C-return>" . er/expand-region)
         ("C--"        . er/contract-region))
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
  :defer t ; (2 global-undo-tree-mode t) ; load after 2 idle secs
  :diminish " ⅄"
  :bind (("C-x u" . undo-tree-visualize) ;; default keybinding
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

(use-package markdown-mode :defer t :ensure t
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  :init
  (use-package markdown-mode+ :defer t :ensure t))

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
  (defvar mode-line-cleaner-alist ; diminishing majore modes
    `((auto-complete-mode       . " α")
      (yas-minor-mode           . " γ")
      (magit-mode               . "ɱ")
      (paredit-mode             . " Φ")
      (eldoc-mode               . "")
      (abbrev-mode              . "")
      (volatile-highlights-mode . " υ")
      (elisp-slime-nav-mode     . " δ")
      (nrepl-mode               . "ηζ")
      (nrepl-interaction-mode   . "ηζ")
      ;; Major modes
      (clojure-mode             . "Cλ")
      (cider-repl-mode          . "C♻")
      (lisp-mode                . " λ")
      (hi-lock-mode             . "")
      (python-mode              . "Py")
      (emacs-lisp-mode          . "ξλ") ; "EL"
      (markdown-mode            . "md")
      (slime-repl-mode          . "π»"))
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
;; TODO test counsel - completion functions using Ivy
;; (use-package counsel :defer t :ensure t ; completion functions using Ivy
;;   :bind (("s-z" . counsel-M-x)
;;          ("C-s-f" . counsel-find-file))
;;   :bind (:map help-map
;;               ("f" . counsel-describe-function)
;;               ("v" . counsel-describe-variable)
;;               ("C-l" . counsel-info-lookup-symbol)))

;; mode-line (pink - bottom left): 'current match/total matches'
(use-package anzu :defer t :ensure t
  :diminish anzu-mode
  :init
  (global-anzu-mode 1)
  (use-package evil-anzu :defer t :ensure t))

(use-package beacon :defer t :ensure t
  :disabled t
  :init (beacon-mode 1)) ; highlight the cursor whenever the window scrolls

(use-package emacs-startup :load-path elisp-dir)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(csv-separators (quote (";")))
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote ("~/dev/webcli")))
 '(evil-search-highlight-persist t t)
 '(git-commit-summary-max-length 70)
 '(global-evil-search-highlight-persist t)
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
 '(paradox-github-token t t)
 '(show-paren-mode t)
 '(tab-width 4))

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

(use-package zenburn-theme :ensure t :defer t
  :if nil ; (boundp 'is-virt-box)
  :init (load-theme 'zenburn 'no-confirm))

(use-package color-theme-solarized :ensure t :defer t
  :if (not (boundp 'is-virt-box))
  :init
  (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark 'no-confirm)
  :config (setq color-theme-is-global t))

;; TODO try out (setq echo-keystrokes 0.4)

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
