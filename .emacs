(setq debug-on-error t) ;; turned off at the end
;; this is for the emacs code browser
(setq stack-trace-on-error t)

(load "server")
(unless (server-running-p)
  (server-start))

(add-to-list 'load-path "~/dev/dotfiles/elisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn-emacs/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized/")

(setq inhibit-splash-screen t)

;; set bash vars http_proxy/https_proxy/ftp_proxy so
;; url-proxy-services won't be needed
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

(use-package auto-package-update
  ;; TODO auto-package-update, use-package :ensure dependend on inet availability
  ;; see :disabled t and :if condition
  :ensure t)

(use-package paredit
  :defer t
  :ensure t
  :init
  (bind-key "s-<left>" 'paredit-backward-slurp-sexp)
  (bind-key "s-<right>" 'paredit-backward-barf-sexp)
  (use-package paredit-menu ; works only with enabled gui elements: see s-f10
    :ensure t))

(use-package ob-clojure ; org-babel-clojure
  :disabled t
  :defer t
  :ensure t
  ;; Attention defaults are:
  ;;     C-c C-l: (cider-load-file FILENAME)
  ;;     C-c C-k: (cider-load-current-buffer)
  )

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

(use-package slamhound
  ;; rip and reconstruct clojure namespace
  :defer t
  :ensure t)

(use-package cider
  :defer t
  :ensure t
  :init

  (use-package kibit-helper
    ;;  kibit - lein plugin for detecting / improving non-idiomatic clj code
    :defer t
    :ensure t
    )
  (use-package cider-eval-sexp-fu
    :defer t
    :ensure t)

  (use-package ac-cider
    :ensure t
    :defer t)

  (use-package rainbow-delimiters
    :ensure t
    :defer t)

  (setq nrepl-log-messages t
        nrepl-hide-special-buffers t
        cider-prefer-local-resources t
        ;; cider-auto-select-error-buffer nil
        ;; cider-stacktrace-default-filters '(tooling dup)
        nrepl-buffer-name-separator "-"
        nrepl-buffer-name-show-port t
        cider-repl-display-in-current-window t
        cider-repl-result-prefix ";; => "
        ;; cider-interactive-eval-result-prefix ";; => "
        ;; cider-repl-use-clojure-font-lock t
        ;; cider-known-endpoints
        ;;       '(("host-a" "10.10.10.1" "7888") ("host-b" "7888"))
        ;; cider-repl-history-file "path/to/file"
        )
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'subword-mode)
  ;; (add-hook 'cider-repl-mode-hook #'paredit-mode)
  ;; (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

  (defun cider-save-and-load-current-buffer ()
    (interactive)
    (when (buffer-modified-p)
      (save-buffer))
    (cider-load-file (buffer-file-name))
    ;; (cider-switch-to-relevant-repl-buffer nil)
    )

  ;; (defun cider-eval-last-expression-in-repl ()
  ;;   "This doesn't work"
  ;;   (interactive)
  ;;   (evil-visual-char)
  ;;   (evil-jump-item)
  ;;   ;; (clipboard-kill-ring-save)
  ;;   ;; (clipboard-kill-region)
  ;;   ;; (cider-switch-to-relevant-repl-buffer)
  ;;   ;; (clipboard-yank)

  ;;   ;; (global-set-key [(shift delete)] 'clipboard-kill-region)
  ;;   ;; (global-set-key [(control insert)] 'clipboard-kill-ring-save)
  ;;   ;; (global-set-key [(shift insert)] 'clipboard-yank)
  ;;   )

  (defun cider-mode-keys ()
    "Modify keymaps used by `cider-mode'."
    (local-set-key (kbd "s-z")
                   ;; 'cider-switch-to-repl-buffer
                   'cider-switch-to-last-clojure-buffer)
    (local-set-key (kbd "s-t") 'cider-test-run-tests)
    (local-set-key (kbd "s-.") 'cider-jump-to-var)
    (local-set-key (kbd "s-,") 'cider-jump-back)
    ;; <menu> key does not work
    ;; (local-set-key (kbd "<menu>-c") 'cider-repl-clear-buffer)
    )
  (add-hook 'cider-mode-hook 'cider-mode-keys)

  ;; (defun cider-interaction-mode-keys ()
  ;;   "Modify keymaps used by `cider-interaction-mode'."
  ;;   ;; (local-set-key (kbd "s-o") 'cider-jump)
  ;;   )
  ;; (add-hook 'cider-interaction-mode-hook 'cider-interaction-mode-keys)
  )

(use-package clojure-mode
 :defer t
 :ensure t
 :init
 (clojure-mode)
 (defun repl-mode-keys ()
   "Modify keymaps used by `repl-mode'."
   (local-set-key (kbd "C-s-j") 'cider-jack-in)
   (local-set-key (kbd "s-r") 'cider-eval-last-expression-in-repl)
   (local-set-key (kbd "s-e") 'cider-eval-last-sexp)
   (if (featurep 'evil-leader)
     (evil-leader/set-key "e" 'cider-eval-last-sexp))
   (local-set-key (kbd "s-z") 'cider-switch-to-repl-buffer)
   (local-set-key (kbd "s-l") 'cider-save-and-load-current-buffer)
   (local-set-key (kbd "s-n") 'cider-repl-set-ns)
   (local-set-key (kbd "s-t") 'cider-test-run-tests)
   (local-set-key (kbd "s-.") 'cider-find-var)
   (local-set-key (kbd "s-,") 'cider-jump-back)
   (local-set-key (kbd "M-m") '(lambda ()
                                       (interactive)
                                       (end-of-buffer)
                                       (message "(-main \"-a\")"))))
 (add-hook 'repl-mode-hook 'repl-mode-keys))

(use-package clj-refactor
  :defer t
  :ensure t
  :init
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (yas-minor-mode 1) ; for adding require/use/import
              ;; eg. rename files with `C-c C-m rf`.
              (cljr-add-keybindings-with-prefix "C-c C-m"))))

;; hide *nrepl-connection* and *nrepl-server* when switching buffers
;; (setq nrepl-hide-special-buffers t)

(use-package window-purpose
  :defer t
  :ensure t
  :init
  ;; C-c , d: window-purpose
  (bind-key "C-s-d" 'purpose-toggle-window-purpose-dedicated)
  ;; C-c , D: window-buffer
  (bind-key "C-s-D" 'purpose-toggle-window-buffer-dedicated)
  (purpose-mode))

;; (use-package auto-complete-config
;;   :disabled t
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
(use-package company
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package linum-relative
  :defer t
  :ensure t
  :init
  (bind-key "s-n" 'linum-relative-toggle)
  (global-linum-mode t))

;; minibuffer completion incremental feedback
(icomplete-mode 99)  ; turn on icomplete-mode

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package autorevert
  :defer t
  :ensure t
  :init
  ;; reload all buffers when the file is changed
  (global-auto-revert-mode t))

;;(desktop-load-default)
;;(desktop-read)

;; (use-package elpy)

(use-package jedi
  :ensure t
  :defer t
  :init
  (use-package company-jedi
    :ensure t
    :defer t
    :init
    (defun my/python-mode-hook ()
      (add-to-list 'company-backends 'company-jedi))
    (add-hook 'python-mode-hook 'my/python-mode-hook)))

(use-package org
  ;; :defer t - can't be done, org-mode is loaded by default
  :ensure t
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
(use-package git-timemachine
  :ensure t
  :defer t)

(use-package magit
  :defer t
  :ensure t
  :init
  (use-package magit-popup
    :defer t
    :ensure t)
  (bind-key "s-m" 'magit-status)
  (setq magit-auto-revert-mode t)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (autoload 'magit-status "magit" nil t))

;; prevent: Error saving to X clipboard manager.
(setq x-select-enable-clipboard-manager nil)

(load-library "environment-lib")
(set-face-attribute 'default nil :height (get-font-height))

;; -t: semicolon is the command line terminator.
;; default is end-of-line as a SQL statement terminator
;; (setq sql-db2-options '("-c" "-i" "-w" "db2" "-tv"))

;; simppe is probably the base package emacs; can't use :ensure t
(use-package simple
  :init
  (size-indication-mode 1)  ; filesize indicator
  (setq truncate-lines t) ;; no line wrap
  (define-key global-map [f5] 'toggle-truncate-lines)
  (column-number-mode 1)

  (defun kill-line-backward (arg)
    "Kill ARG lines backward."
    (interactive "p")
    (kill-line (- 1 arg)))

  ;; (global-set-key (kbd "<S-delete>") 'kill-line)
  (global-set-key (kbd "<S-delete>") 'kill-region)
  ;; clipboard-kill-region belongs to package menu-bar
  ;; (global-set-key (kbd "<S-delete>") 'clipboard-kill-region)

  ;; C-s-backspace is the default key binding for kill-whole-line
  (global-set-key (kbd "<C-s-backspace>") 'kill-line-backward)
  (global-set-key (kbd "<C-S-delete>") 'kill-line)
  ;; (define-key global-map [f5] 'toggle-truncate-lines)
  )

(use-package neotree
  :defer t
  :ensure t
  :init
  (bind-key "<s-f8>" 'neotree-toggle))

;; (use-package icicles ; TODO try out
;;  :ensure t
;;  :defer t)

(use-package helm
  :defer t
  :ensure t
  ;; :pin melpa-stable
  :init

  ;; (use-package helm-dictionary ; local offline dictionaries
  ;;  :ensure t
  ;;  :defer t)

  ;; (use-package helm-themes
  ;;  :ensure t
  ;;  :defer t)

  (use-package cljr-helm
    :defer t
    :ensure t
    :init
    ;; TODO bind `cljr-helm` to a key (I'd suggest C-c r) in Clojure mode
    )

  (use-package helm-commandlinefu
   :ensure t
   :defer t)

  (use-package helm-ack
   :ensure t
   :defer t)

  (use-package helm-cider-history
   :ensure t
   :defer t)

  (use-package helm-google ; alternatively use google-this
    :defer t
    :ensure t
    ;; :pin melpa-stable
    :init

    (use-package google-this
     :defer t
     :ensure t
     :init
     (bind-key "s-g" 'google-this))

    ;; (when (executable-find "curl")
    ;;   (setq helm-google-suggest-use-curl-p t))
    (bind-key "s-G" 'helm-google-suggest)  ; google auto-complete
    ;; helm-google does not work
    ;; (bind-key "s-g" 'helm-google)          ; alternative to google-this region
    )

  ;; ee ace-jump-buffer
  (bind-key "M-x" 'helm-M-x)
  ;; (bind-key "s-u" 'helm-surfraw) ; web search for PATTERN with search ENGINE
  (bind-key "s-p" 'helm-projectile)
  (bind-key "s-a" 'helm-buffers-list)
  (bind-key "C-x b" 'helm-mini)
  (bind-key "s-b" 'helm-mini)
  (bind-key "M-y" 'helm-show-kill-ring)

  (use-package helm-flycheck
    :defer t
    :ensure t
    ;; :pin melpa-stable
    :init
    (eval-after-load 'flycheck
      '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck)))

  (use-package helm-projectile
    :defer t
    ;; :pin melpa-stable
    :ensure t)

  ;; (use-package persp-mode
  ;;  :disabled t ; persp-mode is completely broken
  ;;  :init
  ;;  (with-eval-after-load "persp-mode-autoloads"
  ;;    (setq wg-morph-on nil) ;; switch off animation
  ;;    (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
  ;;  ;; (persp-mode 1)
  ;;  )

  ;; To enable Projectile only in select modes:
  ;; (add-hook 'ruby-mode-hook 'projectile-mode)
  (use-package persp-projectile
    :defer t
    :ensure t
    ;; :pin melpa-stable
    :init
    (bind-key "C-s-p" 'helm-projectile-ack)
    ;; (desktop-save-mode 1)
    ;; TODO save perspective
    (use-package perspective
      :defer t
      :ensure t
      ;; :pin melpa-stable
      :init
      (persp-mode))

    (projectile-global-mode)
    ;; (helm-projectile-on)
    )

  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  ;; rebind tab to do persistent action
  ;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; make TAB works in terminal
  ;; (define-key helm-map (kbd "C-i")   'helm-execute-persistent-action)
  ;; list actions using C-z
  ;; (define-key helm-map (kbd "C-z")   'helm-select-action)

  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t
        helm-locate-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-lisp-fuzzy-completion t
        ;; Open helm buffer inside current window.
        ;; Don't occupy whole other window
        helm-split-window-in-side-p           t
        ;; move to end/beginning of source when reaching top/bottom of source
        helm-move-to-line-cycle-in-source     t
        ;; search for library in `require' and `declare-function' sexp.
        helm-ff-search-library-in-sexp        t
        ;; scroll 8 lines other window using M-<next>/M-<prior>
        helm-scroll-amount                    8
        helm-ff-file-name-history-use-recentf t)

  (helm-mode 1)
  (helm-autoresize-mode 1))

(use-package drag-stuff
  :defer t
  :ensure t
  :init
  (drag-stuff-global-mode t))

(use-package vimrc-mode
  :defer t
  :ensure t
  :init
  (require 'vimrc-mode)
  (add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode)))

(use-package dired ; not among *Packages*; can't use :ensure t
  :defer t
  :init

  (bind-key "s-d" 'dired-jump)
  (bind-key "s-r" 'dired-do-rename dired-mode-map)

  (use-package dired-details
    :defer t
    :ensure t ; try :ensure marmalade if not available on melpa
    :init
    (require 'dired-details)
    (dired-details-install))

  (use-package
   dired-details+
   :defer t
   :ensure t
   :init
   (use-package dired+ ;; show / hide file details: ( / )
    :defer t
    :ensure t
    :init
    (require 'dired+))
   (require 'dired-details+))

  (use-package dired-subtree
    :defer t
    :ensure t
    :init
    (define-key dired-mode-map (kbd "<C-return>") 'dired-subtree-insert)
    (define-key dired-mode-map (kbd "<C-M-return>") 'dired-subtree-remove))

  (use-package dired-rainbow ; file highlighting
    :defer t
    :ensure t)
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


(use-package winner ; layout management
  :defer t
  :ensure t
  :init
  (winner-mode 1))

(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/shorten-directory t
        ;; sml/theme 'respectful
        sml/name-width 32
        sml/shorten-modes t
        sml/use-projectile-p 'before-prefixes
        sml/projectile-replacement-format "%s/")
  (add-hook 'after-init-hook 'sml/setup))

(use-package evil
  :ensure t
  :init
  (evil-mode 1)

  (bind-key "s-SPC" 'evil-search-highlight-persist-remove-all)
  (bind-key "C-s-t" 'evil-mode)
  (bind-key "s-;" 'evilnc-comment-or-uncomment-lines)
  (bind-key "s-z" 'evil-ace-jump-char-mode)

  ;; require for evil folding
  (add-hook 'prog-mode-hook 'hs-minor-mode)

  (use-package evil-visual-mark-mode ; TODO see helm-bookmarks
    :ensure t
    :defer t
    :init
    (evil-visual-mark-mode))

  ;; (interactive "r")
  ;; TODO backspace smartparens
  ;; (define-key evil-insert-state-map "<backspace>" 'evil-delete)
  ;; (define-key evil-insert-state-map "<delete>" 'evil-delete)
  ;; (message "evil-mode 1")

  ;; f/F/t/T; emulates vim-sneak, vim-seek for evil-mode by default
  ;; bound to s/S in normal mode and z/Z/x/X in visual or operator mode.
  ;; (use-package evil-snipe
  ;;   :config
  ;;   (progn
  ;;     (global-evil-snipe-mode 1)))

  ;; text exchange operator
  ;;(use-package evil-exchange) ; not available in melpa-stable
  ;;(setq evil-exchange-key (kbd "zx"))
  ;;(evil-exchange-install)

  (use-package evil-nerd-commenter
    :ensure t
    :init
    (bind-key "C-;" 'evilnc-comment-or-uncomment-lines)
    (bind-key "M-;" 'evilnc-comment-or-uncomment-lines))

  (use-package evil-visualstar
    :ensure t
    :defer t)

  ;; (use-package evil-jumper) ;; C-i / C-o

  ;; Set cursor colors depending on mode
  (when (display-graphic-p)
    (setq evil-emacs-state-cursor '("red" box)
          evil-normal-state-cursor '("green" box)
          evil-visual-state-cursor '("orange" box)
          evil-insert-state-cursor '("red" bar)
          evil-replace-state-cursor '("red" bar)
          evil-operator-state-cursor '("red" hollow)))

  (use-package evil-args
    :ensure t
    :init

    (use-package evil-surround
      :ensure t
      :init
      (defun visual-double-quote-string (&optional arg)
        "Select string inside double quote chars"
        (evil-normal-state)
        (interactive "p")
        (kmacro-exec-ring-item (quote ("vi\"" 0 "%d")) arg))

      (bind-key "s-\"" 'visual-double-quote-string)
      (bind-key "M-\"" 'visual-double-quote-string)
      (global-evil-surround-mode 1))

    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

    ;; bind evil-forward/backward-args
    (define-key evil-normal-state-map "L" 'evil-forward-arg)
    (define-key evil-normal-state-map "H" 'evil-backward-arg)
    (define-key evil-motion-state-map "L" 'evil-forward-arg)
    (define-key evil-motion-state-map "H" 'evil-backward-arg)

    ;; bind evil-jump-out-args
    (define-key evil-normal-state-map "K" 'evil-jump-out-args))

  (use-package evil-numbers
    :defer t
    :ensure t
    :init
    (bind-key "C-c +" 'evil-numbers/inc-at-pt)
    (bind-key "C-c -" 'evil-numbers/dec-at-pt)
    (bind-key "s-+" 'evil-numbers/inc-at-pt)
    (bind-key "s--" 'evil-numbers/dec-at-pt)
    (bind-key (kbd "<C-kp-add>")       'evil-numbers/inc-at-pt)
    (bind-key (kbd "<C-kp-subtract>")  'evil-numbers/dec-at-pt)
    (bind-key (kbd "<s-kp-add>")       'evil-numbers/inc-at-pt)
    (bind-key (kbd "<s-kp-subtract>")  'evil-numbers/dec-at-pt))

  (use-package evil-smartparens
    :defer t
    :ensure t
    :init
    ;; evil-smartparens everywhere
    ;; (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
    ;; evil-smartparens only in clojure
    (add-hook 'clojure-mode-hook #'evil-smartparens-mode)
    ;; (sp-pair "\{" "\}")
    )

  ;; enable global-evil-leader-mode before evil-mode, otherwise
  ;; evil-leader won’t be enabled in the initial buffers
  ;; (*scratch*, *Messages*, ...)
  (use-package evil-leader
    :ensure t
    :defer t
    :init
    (global-evil-leader-mode)
    (setq evil-leader/in-all-states t)

    (defun delete-rest-of-cheatsheet-entry (&optional arg)
      "kbd macro - starts in evil-normal-mode;
      delete up to the ' (single quote) character"
      (interactive "p")
      (kmacro-exec-ring-item (quote ([118 116 39 120 105] 0 "%d"))
                             arg))

    (evil-leader/set-key
      "dd" 'kill-whole-line
      ;; wr gives: (error "Key sequence w r starts with non-prefix key w")
      ;; "wr" 'toggle-truncate-lines
      "q" 'other-window
      "dr" 'delete-rest-of-cheatsheet-entry
      "SPC" 'evil-search-highlight-persist-remove-all)

    (if (featurep 'helm)
        (evil-leader/set-key
          ;; x gives: (error "Key sequence x x starts with non-prefix key x")
          ;; "x" 'helm-M-x
          ;; this doesn't help:
          ;;   (eval-after-load 'helm "x" 'helm-M-x)
          ;; although manual eval "after" helps
          "G" 'helm-google-suggest ; google auto-complete
          "g" 'helm-google         ; alternative to google-this region
          "f" 'helm-find-files
          "a" 'helm-buffers-list)
      (evil-leader/set-key
        ;; "x" 'execute-extended-command
        "f" 'find-file
        "a" 'switch-to-buffer)))

  ;; from evil-commands
  (define-key evil-normal-state-map (kbd "<C-O>") 'evil-jump-forward)

  (use-package evil-search-highlight-persist
    :ensure t
    :init
    (global-evil-search-highlight-persist t))

  ;; mode-line (pink - bottom left): 'current match/total matches'
  (use-package anzu
    :defer t
    :ensure t
    :diminish anzu-mode
    :init
    (global-anzu-mode 1)
    (use-package evil-anzu
      :ensure t)))

(use-package transpose-frame
  :load-path "~/dev/dotfiles/elisp/transpose-frame"
  :defer t
  :ensure t
  :init
  (require 'transpose-frame) ; neccessary - otherwise: transpose-frame undefined
  ;; TODO check how to automate byte-compilation of transpose-frame
  (bind-key "<f8>" 'transpose-frame))

(use-package time
  :ensure t
  :init
  (setq display-time-24hr-format 1)
  (display-time-mode 1))

(defun back-window ()
  ;; opposite of other-window
  (interactive)
  (other-window -1))

(defun package-auto-upgrade ()
  (interactive)
  (package-list-packages)
  (package-menu-mark-obsolete-for-deletion)
  (package-menu-mark-upgrades)
  (package-menu-execute))

(use-package paradox
  :defer t
  :ensure t
  :init

  (use-package spinner
    :defer t
    :ensure t)

  (bind-key [f9]  'paradox-list-packages) ; TODO auto enable/disable evil-mode
  (bind-key (kbd "<s-f9>") 'paradox-upgrade-packages)
  (setq paradox-github-token (getenv "GITHUB_TOKEN")
        paradox-automatically-star t))

;; pretty syntax highlighting everywhere
(global-font-lock-mode t)

;; (setq inferior-lisp-program "browser-repl")
;; (setq inferior-lisp-program "cljs-repl")
;; (message (concat "inferior-lisp-program: " inferior-lisp-program))

;; TODO is cljs-repl a delete cantidate? See bash/cljs-repl
(defun cljs-repl ()
  (interactive)
  (setq inferior-lisp-program "cljs-repl")
  (run-lisp))

(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
    major-mode))

;; (setq inferior-lisp-buffer "browser-repl")
;; (message inferior-lisp-buffer)

;; edit every instance of word/variable in the buffer - like multiple cursors
(use-package iedit
  :defer t
  :ensure t
  :init
  (bind-key "s-i" 'iedit-mode))

(use-package multiple-cursors
  :defer t
  :ensure t
  :bind
  (("C->" . mc/mark-all-like-this-in-defun)
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

(use-package ace-jump-mode
  :ensure t
  :defer t
  :init
  (autoload 'ace-jump-mode "ace-jump-mode" nil t)

  (bind-key "<f2>" 'ace-jump-mode)
  (bind-key "s-j" 'ace-jump-mode)
  ;; (bind-key "s-a" 'ace-jump-buffer) ; see helm-buffers-list
  (bind-key "<C-f2>" 'ace-jump-line-mode)

  (when (and (featurep 'evil) (featurep 'evil-leader))
    (evil-leader/set-key
      "c" 'ace-jump-char-mode
      "w" 'ace-jump-word-mode
      "l" 'ace-jump-line-mode)))

(use-package expand-region
  :defer t
  :ensure t
  :bind ("C-=" . er/expand-region)
  :init
  (when (and (featurep 'evil) (featurep 'evil-leader))
    (progn
      (setq expand-region-contract-fast-key "z")
      (evil-leader/set-key "xx" 'er/expand-region))))

(use-package yasnippet
  :defer t
  :ensure t
  :init
  (let ((yasnippet-dir "~/.emacs.d/plugins/yasnippet"))
    (shell-command-to-string (concat "mkdir -p " yasnippet-dir))
    (add-to-list 'load-path yasnippet-dir))
  (yas-global-mode 1)
  ;; (define-key yas-minor-mode-map (kbd "s-y") 'yas/expand)
  ;; (define-key yas-minor-mode-map (kbd "TAB") nil)
  )

(use-package edit-server
  ;; Repond to requests from the Emacs Chrome plugin using sockets.
  ;; See emacs activation docu in the browser plugin
  :disabled t
  :defer t
  :if window-system
  :init
  (add-to-list 'load-path "~/.emacs.d")
  (setq edit-server-url-major-mode-alist
        '(("github\\.com" . markdown-mode)
          ("github\\.com" . md)))
  (setq edit-server-new-frame nil)
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(use-package browse-url
  :defer t
  :ensure t
  :init
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program
        ;; "chromium-browser" does not work properly on ubuntu 13.10
        ;; "chrome" ; cygwin
        "google-chrome"))

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; xfce4-settings-manager -> Window Manger -> Keyboard -> ...
(use-package duplicate-thing
  :defer t
  :ensure t
  :init
  (bind-key (kbd "C-M-<up>") 'duplicate-thing)
  (bind-key (kbd "C-M-<down>") 'duplicate-thing))

;; (defun ignore-error-wrapper (fn)
;;   "Funtion return new function that ignore errors.
;;    The function wraps a function with `ignore-errors' macro."
;;   (lexical-let ((fn fn))
;;     (lambda ()
;;       (interactive)
;;       (ignore-errors
;;         (funcall fn)))))

(use-package minimap
  :defer t
  :ensure t
  :bind ("s-i" . minimap-toggle))

;; (load-library "abbrev-table")
;;(global-set-key [f11] 'abbrev-mode)
;;(global-set-key [f11] 'toggle-frame-fullscreen) ; this is the default
(toggle-frame-maximized)

(use-package undo-tree
  :ensure t
  :defer (2 global-undo-tree-mode t) ; load after 2 seconds of idle time
  :diminish ""
  :config
  (define-key undo-tree-map (kbd "C-x u") 'undo-tree-visualize) ;; default
  (define-key undo-tree-map (kbd "<f12>") 'undo-tree-visualize)
  (define-key undo-tree-map (kbd "C-/") 'undo-tree-undo))

;; (global-set-key [scroll] 'exec-test-macro)

(defun switch-to-buffer-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(global-set-key (kbd "<s-f12>") 'switch-to-buffer-scratch)

(unless (display-graphic-p)
  (use-package evil-terminal-cursor-changer
    :defer t
    :ensure t))

;; (define-key global-map [(control ?z) ?u] 'uniq-lines)

(use-package ace-window
  :defer t
  :ensure t
  :init
  (global-set-key (kbd "M-o") 'ace-window)
  ;; the sequence of leading characters for each window:
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package sublimity
  :ensure t
  :init
  ;; only smooth-scrolling together with sublimity leads to
  ;; smooth scrolling really working! WTF?
  (use-package smooth-scrolling
    :ensure t)
  (use-package sublimity-scroll); inside sublimity :ensure t not needed
  (sublimity-mode 1))

;; another possibility how to define a key chord:
;; (global-set-key [(control x) (?0)] 'sticky-window-delete-window)
(use-package sticky-windows
  ;; sticky-windows must by downloaded from
  ;; http://www.emacswiki.org/emacs/download/sticky-windows.el
  ;; :ensure t
  :init
  (bind-key "C-x 0" 'sticky-window-delete-window)
  (bind-key "C-x 1" 'sticky-window-delete-other-windows)
  (bind-key "C-x 9" 'sticky-window-keep-window-visible))

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

(use-package whitespace
  :defer t
  :ensure t
  :diminish whitespace-mode
  :init
  (bind-key "s-w" 'whitespace-mode)
  (bind-key "s-<f7>" '(lambda ()
                       (whitespace-cleanup)
                       (interactive)
                       (message "whitespace-cleanup done.")))
  (setq require-final-newline t)
  (set-default 'indicate-empty-lines t)
  (setq show-trailing-whitespace t))

;; Always prefer to load newer files,
;; instead of giving precedence to the .elc files
(setq load-prefer-newer t)

(use-package popwin
  :defer t
  :ensure t
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

(use-package color-identifiers-mode
  ;; TODO disable color-identifiers-mode only for specific modes: clojure-mode
  :disabled t ; color-identifiers-mode is nice but noisy
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-color-identifiers-mode)

  (defun my/enable-color-identifiers ()
    (interactive)
    (color-identifiers-mode t)))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

(use-package markdown-mode
  :ensure t
  :init
  (add-hook 'markdown-mode-hook
            (lambda () (electric-indent-local-mode -1))))

(use-package mmm-mode
  :ensure t
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

(use-package yagist
  ;; TODO yagist RSA fingerprint
  ;; # The authenticity of host 'gist.github.com (192.30.252.141)' can't be established.
  ;; # RSA key fingerprint is ...
  ;; # Are you sure you want to continue connecting (yes/no)? yes
  :ensure t
  :defer t
  :init
  (setq yagist-github-token (getenv "GITHUB_TOKEN")))

(use-package fish-mode
  :ensure t
  :defer t)

(use-package emacs
  :ensure t
  :init

  (bind-key "C-<f11>" '(lambda ()
                         (interactive)
                         (shell-command
                          ;; "cvs-ci-hooks.sh"
                          "")))

  (bind-key "C-<f8>" '(lambda ()
                        (interactive)
                        (shell-command
                         ;; "cvs-test.sh -lo :pserver:faizal@localhost:/myrepos"
                         "")))
  (bind-key "C-<f12>" '(lambda ()
                         (interactive)
                         (shell-command
                          ;; (concat "cvs-test.sh -fr "
                          ;;         ":pserver:"
                          ;;         "rsvoboda@dlnxcvshooksdev01.ptx.fr.sopra"
                          ;;         ":2401/cvscorfja")
                          "")))

  (bind-key (kbd "<s-f3>") 'kmacro-start-macro-or-insert-counter)
  (bind-key (kbd "<s-f4>") 'kmacro-end-or-call-macro)

  (use-package grep+
    :defer t
    :ensure t
    :init
    (require 'grep+))

  (bind-key [f3] 'find-grep) ; Use -e '...' instead of -e "..."
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

  (bind-key "s-r" 'rename-file-and-buffer)

  ;; (global-set-key (kbd "s-i")
  ;;                 '(lambda ()
  ;;                    (interactive)
  ;;                    (insert "git --git-dir=../credit.git/ ")))

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

  ;; (global-set-key [f10] 'menu-bar-open)     ;; this is the default
  (bind-key (kbd "<s-f10>") 'gui-toggle) ;; shows also scrollbars

  (bind-key "s-s" 'save-buffer)
  (bind-key "s-f" 'find-file)
  (bind-key "s-c" 'kill-ring-save) ; copy
  (bind-key "s-x" 'kill-region)    ; cut
  (bind-key "s-v" 'yank)           ; paste
  ;; see evil-window-map
  ;; TODO s-q should work like C-tab if only one window is visible
  (bind-key "s-q" 'other-window)
  (bind-key (kbd "<s-tab>") 'other-window)

  (bind-key "s-0" 'delete-window)
  (bind-key "s-1" 'delete-other-windows)
  (bind-key "s-2" 'split-window-below)
  (bind-key "s-3" 'split-window-right)

  (bind-key "s-h" 'describe-key)
  (bind-key "s-k" 'close-buffer)
  (bind-key "C-s-k" 'delete-file-and-close-its-buffer)

  ;; (bind-key [C-s-left] (ignore-error-wrapper 'windmove-left))
  ;; (bind-key [C-s-right] (ignore-error-wrapper 'windmove-right))
  ;; (bind-key [C-s-up] (ignore-error-wrapper 'windmove-up))
  ;; (bind-key [C-s-down] (ignore-error-wrapper 'windmove-down))

  ;; (bind-key "s-b" 'ido-switch-buffer) ; s-b used for helm-mini
  ;; cycle through buffers with Ctrl-Tab / Shift-Ctrl-Tab
  (bind-key (kbd "<C-tab>") 'bury-buffer)
  (bind-key (kbd "<C-S-iso-lefttab>") 'unbury-buffer)
  (bind-key (kbd "C-`") 'unbury-buffer)

  (bind-key [M-s-left] 'shrink-window-horizontally)
  (bind-key [M-s-right] 'enlarge-window-horizontally)
  (bind-key [M-s-down] 'enlarge-window)
  (bind-key [M-s-up] 'shrink-window)
  (bind-key [f7] 'find-file-emacs)
  (bind-key (kbd "<s-f11>") 'find-file-emacs)
  (bind-key (kbd "<C-kp-multiply>") 'highlight-symbol-at-point)

  ;; (setq default-directory "~/dev")

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
    (find-file "~/dev/dotfiles/.emacs")   ; open .emacs/other user init file
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
        (evil-leader/set-key "e" 'eval-last-sexp))
    ;; see ("s-u" . helm-surfraw)
    (local-set-key (kbd "s-u")
                   'eval-buffer
                   ;; TODO s-u: should print "... eval-buffer done"
                   ;; (lambda ()
                   ;;   (eval-buffer)
                   ;;   (message (concat (buffer-name) ": eval-buffer done.")))
                   ))
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


 ;; TODO use git submodules for themes
 ;; activate following when using (load-theme 'solarized t)
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(frame-background-mode (quote dark))


 '(ecb-options-version "2.40")
 '(ecb-source-path (quote ("~/dev/webcli")))
 '(evil-search-highlight-persist t t)
 '(git-commit-summary-max-length 70)
 '(global-evil-search-highlight-persist t)
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
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

(load-theme 'zenburn t)
;; (disable-theme 'zenburn)  (enable-theme 'zenburn)

;; (load-theme 'solarized t)
;; (disable-theme 'solarized)  (enable-theme 'solarized)

(setq debug-on-error nil)
