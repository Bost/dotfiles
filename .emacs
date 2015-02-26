(setq debug-on-error t) ;; turned off at the end
;; this is for the emacs code browser
(setq stack-trace-on-error t)

;; TODO jump to last line and switch to edit mode
;; TODO delete to the end of line and switch to edit mode

;; TODO emacs does not come up when calling emacsclient withouth emacs server
(load "server")
(unless (server-running-p)
  (server-start))

(add-to-list 'load-path "~/dev/dotfiles/elisp/")
(load-library "style-lib")

;; url-proxy-services not needed if bash vars http_proxy/https_proxy/ftp_proxy are set
(if (string= system-type "windows-nt")
    (setq url-proxy-services '(("no_proxy" . "work\\.com")
                               ("http" . "ptx.proxy.corp.sopra:8080")
                               ("https" . "ptx.proxy.corp.sopra:8080"))))

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ))
;; activate all the packages (in particular autoloads)
(package-initialize)
;; (package-refresh-contents)

(require 'use-package)

(use-package anzu-mode ;; mode-line  (pink - bottom left): 'current match/total matches'
  :init
  (progn
    (global-anzu-mode +1)))

(use-package paredit
  :bind (((kbd "s-<left>")  . paredit-backward-slurp-sexp)
         ((kbd "s-<right>") . paredit-backward-barf-sexp)))

(use-package paredit-menu)

(use-package evil-smartparens
  :init
  (progn
    ;; evil-smartparens everywhere
    ;; (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
    ;; evil-smartparens only in clojure
    (add-hook 'clojure-mode-hook #'evil-smartparens-mode)
    ;; (sp-pair "\{" "\}")
    ))

;; org-babel-clojure
;; (use-package ob-clojure)
;; Attention defaults are:
;;     C-c C-l: (cider-load-file FILENAME)
;;     C-c C-k: (cider-load-current-buffer)

;;(defun skewer-mode-keys ()
;;  "Modify keymaps used by `skewer-mode'."
;;  (local-set-key (kbd "s-e") 'skewer-eval-last-expression)
;;  (local-set-key (kbd "s-x") 'skewer-eval-defun)
;;  (local-set-key (kbd "s-l") 'skewer-load-buffer)
;;  )
;; skewer works on top of js2-mode
;; (add-hook 'js2-mode-hook 'skewer-mode-keys)
;; (add-hook 'skewer-mode-hook 'skewer-mode-keys)

(load-library "cider-lib")

;; hide *nrepl-connection* and *nrepl-server* when switching buffers
;; (setq nrepl-hide-special-buffers t)

(use-package auto-complete-config
  :init
  (progn
    (ac-config-default)))

(use-package linum-relative
  :bind ("s-n" . linum-relative-toggle)
  :init
  (progn
    (global-linum-mode t)))

;; minibuffer completion incremental feedback
(icomplete-mode 99)  ; turn on icomplete-mode

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package autorevert
  :init
  (progn
    ;; reload all buffers when the file is changed
    (global-auto-revert-mode t)))

;;(desktop-load-default)
;;(desktop-read)

(use-package org
  :config
  (progn
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
    ))


;; Setup custom shortcuts
;;(global-set-key "\C-x\C-g" 'goto-line)
;;(global-set-key [f1] 'compile)
;;(global-set-key [f2] 'next-error)

;;(add-to-list 'load-path "~/.emacs.d/edit-server/")

;; (global-set-key [f6] 'split-window-horizontally)
(use-package magit
  ;; :bind ("M-g M-g" . magit-status)
  :bind
  ("<f6>" . magit-status) ;; [f6] does not work
  ("s-m" . magit-status)
  :init
  (progn
    (autoload 'magit-status "magit" nil t))
  ;; :config
  ;; (progn
  ;;   (when (eq system-type 'darwin)
  ;;     (setq magit-emacsclient-executable "/usr/local/bin/emacsclient"))
  ;;   (defun magit-browse ()
  ;;     "Browse to the project's github URL, if available"
  ;;     (interactive)
  ;;     (let ((url (with-temp-buffer
  ;;                  (unless (zerop (call-process-shell-command
  ;;                                  "git remote -v" nil t))
  ;;                    (error "Failed: 'git remote -v'"))
  ;;                  (goto-char (point-min))
  ;;                  (when (re-search-forward
  ;;                         "github\\.com[:/]\\(.+?\\)\\.git" nil t)
  ;;                    (format "https://github.com/%s" (match-string 1))))))
  ;;       (unless url
  ;;         (error "Can't find repository URL"))
  ;;       (browse-url url)))

  ;;   (when (and (boundp 'moe-theme-which-enabled)
  ;;              (eq moe-theme-which-enabled 'dark))
  ;;     ;; Moe's magit colors are baaaaaaad
  ;;     (set-face-attribute 'magit-item-highlight nil
  ;;                         :inherit nil
  ;;                         :foreground 'unspecified))

  ;;   (define-key magit-mode-map (kbd "C-c C-b") 'magit-browse)
  ;;   (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
  ;;   (custom-set-variables '(magit-set-upstream-on-push (quote dontask)))
  ;;   (setq magit-completing-read-function 'magit-ido-completing-read)
  ;;   ;; Diminish the auto-revert-mode
  ;;   (add-hook 'magit-auto-revert-mode-hook
  ;;             (diminish 'magit-auto-revert-mode)))
 )

(setq x-select-enable-clipboard-manager nil) ; prevent: Error saving to X clipboard manager.

(load-library "environment-lib")
(set-face-attribute 'default nil :height (get-font-height))

;; highlight current line - this is probably not needed in the default face
;; (global-hl-line-mode 1)

;; -t: semicolon is the command line terminator.
;; default is end-of-line as a SQL statement terminator
;; (setq sql-db2-options '("-c" "-i" "-w" "db2" "-tv"))

(use-package simple
  :init
  (progn
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
    (global-set-key (kbd "<C-s-delete>") 'kill-line)
    ;; (define-key global-map [f5] 'toggle-truncate-lines)
    ))

(use-package neotree
  :bind ("<s-f8>" . neotree-toggle))

;; (desktop-save-mode 1)

;; (defun save-macro (name)
;;     "save a macro. Take a name as argument
;;      and save the last defined macro under
;;      this name at the end of your .emacs"
;;      (interactive "SName of the macro :")  ; ask for the name of the macro
;;      (kmacro-name-last-macro name)         ; use this name for the macro
;;      (find-file "~/dev/dotfiles/.emacs")   ; open .emacs or other user init file
;;      (goto-char (point-max))               ; go to the end of the .emacs
;;      (newline)                             ; insert a newline
;;      (insert-kbd-macro name)               ; copy the macro
;;      (newline)                             ; insert a newline
;;      (switch-to-buffer nil))               ; return to the initial buffer


(use-package helm-config ;; or just helm ?
  :bind (("M-x" . helm-M-x)
         ("s-g" . helm-google-suggest)
         ("s-u" . helm-surfraw)
         ("s-p" . helm-projectile)
         ;; see ace-jump-buffer
         ("s-a" . helm-buffers-list))
  :init
  (progn
    (progn ;; helm-M-x
      (setq helm-M-x-fuzzy-match t))

    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))


    ;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ;; rebind tab to do persistent action
    ;; (define-key helm-map (kbd "C-i")   'helm-execute-persistent-action) ;; make TAB works in terminal
    ;; (define-key helm-map (kbd "C-z")   'helm-select-action) ;; list actions using C-z
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t)

    (progn ;; helm-mini
      (setq helm-buffers-fuzzy-matching t
            helm-recentf-fuzzy-match    t)
      (global-set-key (kbd "C-x b") 'helm-mini)
      (global-set-key (kbd "s-b") 'helm-mini))

    (progn ;; helm-locate
      (setq helm-locate-fuzzy-match t))
    (progn ;; helm-semantic
      (setq helm-semantic-fuzzy-match t))
    (progn ;; helm-imenu
      (setq helm-imenu-fuzzy-match t))
    (progn ;; helm-apropos
      (setq helm-apropos-fuzzy-match t))
    (progn ;; helm-lisp-completion-at-point
      (setq helm-lisp-fuzzy-completion t))

    (helm-mode 1)
    (helm-autoresize-mode 1)))

(use-package move-text
  :init
  (progn
    ;; Bind move-text-up/down to M-up/down
    (move-text-default-bindings)))

(put 'upcase-region 'disabled nil)

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

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

(use-package winner ;; layout management
  :init
  (progn
    (winner-mode 1)))

(load-library "evil-numbers-lib")

;; enable global-evil-leader-mode before evil-mode, otherwise
;; evil-leader won’t be enabled in initial buffers (*scratch*, *Messages*, ...)
(use-package evil-leader
  :init
  (progn
    (global-evil-leader-mode)
    (setq evil-leader/in-all-states t)
    (evil-leader/set-key
      "wr" 'toggle-truncate-lines
      "dd" 'kill-whole-line
      "SPC" 'evil-search-highlight-persist-remove-all)

    (if (featurep 'helm)
        (evil-leader/set-key
          "f" 'helm-find-files
          "a" 'helm-buffers-list)
      (evil-leader/set-key
        "f" 'find-file
        "a" 'switch-to-buffer))
    ))

(use-package evil
  :bind ("s-t" . evil-mode)
  :init
  (progn
    ;; (interactive "r")
    (evil-mode 1)
    ;; (message "evil-mode 1")
    ))

;; f/F/t/T; emulates vim-sneak, vim-seek for evil-mode by default
;; bound to s/S in normal mode and z/Z/x/X in visual or operator mode.
;; (use-package evil-snipe
;;   :init
;;   (progn
;;     (global-evil-snipe-mode 1)))

(use-package evil-commands
  :init
  (progn
    (define-key evil-normal-state-map (kbd "<C-O>") 'evil-jump-forward)))

(global-set-key (kbd "<C-kp-multiply>") 'highlight-symbol-at-point)

(use-package evil-search-highlight-persist
  :init
  (progn
    (global-evil-search-highlight-persist t)))

(use-package transpose-frame
  :bind ("<f8>" . transpose-frame)
  :init
  (progn
    (add-to-list 'load-path "~/.emacs.d/elpa/transpose-frame/")))

(use-package time
  :init
  (progn
    (setq display-time-24hr-format 1)
    (display-time-mode 1)))

(defun back-window ()
  ;; opposite of other-window
  (interactive)
  (other-window -1))

;; cycle through buffers with Ctrl-Tab / Shift-Ctrl-Tab
(global-set-key (kbd "<C-tab>") 'bury-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'unbury-buffer)

(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; (setq default-directory "~/dev")

(defun package-auto-upgrade ()
  (interactive)
  (package-list-packages)
  (package-menu-mark-obsolete-for-deletion)
  (package-menu-mark-upgrades)
  (package-menu-execute))


(setq gui-elements -1)
(menu-bar-mode gui-elements)
(scroll-bar-mode gui-elements)


(defun gui-toggle ()
  (interactive)
  (setq gui-elements (* -1 gui-elements))
  (menu-bar-mode gui-elements)
  (toggle-scroll-bar gui-elements)
  (message "gui-elements %s"
           (if (= 1 gui-elements) "enabled" "disabled")))

(use-package package
  :bind (("<f9>"   . package-list-packages-no-fetch)
         ("<s-f9>" . package-auto-upgrade)))

;; (global-set-key [f10] 'menu-bar-open)     ;; this is the default
(global-set-key (kbd "<s-f10>") 'gui-toggle) ;; shows also scrollbars

;; pretty syntax highlighting everywhere
(global-font-lock-mode t)

;; (setq inferior-lisp-program "browser-repl")
;; (setq inferior-lisp-program "cljs-repl")
;; (message (concat "inferior-lisp-program: " inferior-lisp-program))

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

;; edit every instance of that word/variable in the buffer - like multiple cursors
(use-package iedit
  :bind ("s-i" . iedit-mode))

(use-package multiple-cursors
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

(use-package evil-integration
  :bind ("s-z" . evil-ace-jump-char-mode))

(use-package ace-jump-mode
  :bind (("<f2>" . ace-jump-mode)
         ("s-j" . ace-jump-mode)
         ;; ("s-a" . ace-jump-buffer) ;; see helm-buffers-list
         )
  :init
  (progn
    (when (and (featurep 'evil) (featurep 'evil-leader))
      (evil-leader/set-key
        "c" 'ace-jump-char-mode
        "w" 'ace-jump-word-mode
        "l" 'ace-jump-line-mode))))

(use-package ace-jump-line-mode
  :bind ("<C-f2>". ace-jump-line-mode))

;; (global-unset-key (kbd "<f3>"))
;; (global-set-key (kbd "<f3>") 'kmacro-start-macro)

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :init
  (progn
    (when (and (featurep 'evil) (featurep 'evil-leader))
      (progn
        (setq expand-region-contract-fast-key "z")
        (evil-leader/set-key "xx" 'er/expand-region)))))

;; yasnippets does not to work
;; (add-to-list 'load-path
;;           "~/.emacs.d/elpa/clojure-snippets-20130403.2046/snippets/clojure-mode")
;; (use-package yasnippet)
;; (yas-global-mode 1)

;; (define-key yas-minor-mode-map (kbd "s-y") 'yas/expand)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)

(use-package browse-url
  :init
  (progn
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program
          ;; "chromium-browser" does not work properly on ubuntu 13.10
          ;; "chrome" ; cygwin
          "google-chrome")))

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; xfce4-settings-manager -> Window Manger -> Keyboard -> ...
(use-package duplicate-thing
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

;; (global-set-key [C-s-left] (ignore-error-wrapper 'windmove-left))
;; (global-set-key [C-s-right] (ignore-error-wrapper 'windmove-right))
;; (global-set-key [C-s-up] (ignore-error-wrapper 'windmove-up))
;; (global-set-key [C-s-down] (ignore-error-wrapper 'windmove-down))

(global-set-key [M-s-left] 'shrink-window-horizontally)
(global-set-key [M-s-right] 'enlarge-window-horizontally)
(global-set-key [M-s-down] 'enlarge-window)
(global-set-key [M-s-up] 'shrink-window)

(use-package minimap
  :bind ("s-i" . minimap-toggle))

;; (load-library "abbrev-table")
;;(global-set-key [f11] 'abbrev-mode)
;;(global-set-key [f11] 'toggle-frame-fullscreen) ; this is the default
(toggle-frame-maximized)

(global-set-key [f7] 'find-file-emacs)
(global-set-key (kbd "s-<f11>") 'find-file-emacs)


(use-package undo-tree
  :idle (global-undo-tree-mode t)
  :diminish ""
  :config
  (progn
    (define-key undo-tree-map (kbd "C-x u") 'undo-tree-visualize) ;; default
    (define-key undo-tree-map (kbd "<f12>") 'undo-tree-visualize)
    (define-key undo-tree-map (kbd "C-/") 'undo-tree-undo)))

;; (global-set-key [scroll] 'exec-test-macro)

(defun switch-to-buffer-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(global-set-key (kbd "<s-f12>") 'switch-to-buffer-scratch)

;; must be loaded in the end; otherwise:
;;    Symbol's function definition is void: mapcar*
;; (use-package workgroups2)

;; (desktop-save-mode t)       ; save all opened files (or disable it)
;; (setq wg-prefix-key (kbd "C-c z")
;;      wg-restore-associated-buffers nil ; all buffers opened in current WG
;;      wg-use-default-session-file nil   ; turn off for "emacs --daemon"
;;      wg-default-session-file "~/.emacs_files/workgroups"
;;      wg-use-faces nil
;;      wg-morph-on nil)    ; animation off
;; (workgroups-mode 1)       ; Activate workgroups

;; (global-set-key (kbd "<pause>")     'wg-reload-session)
;; (global-set-key (kbd "C-S-<pause>") 'wg-save-session)
;; (global-set-key (kbd "s-z")         'wg-switch-to-workgroup)
;; (global-set-key (kbd "s-/")         'wg-switch-to-previous-workgroup)

;; (defun sql-db2-g72 ()
;;  (interactive)
;;  (copy-line -1))


;; (add-to-list 'load-path "path/to/evil-args")
(use-package evil-args
  :init
  (progn
    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

    ;; bind evil-forward/backward-args
    (define-key evil-normal-state-map "L" 'evil-forward-arg)
    (define-key evil-normal-state-map "H" 'evil-backward-arg)
    (define-key evil-motion-state-map "L" 'evil-forward-arg)
    (define-key evil-motion-state-map "H" 'evil-backward-arg)

    ;; bind evil-jump-out-args
    (define-key evil-normal-state-map "K" 'evil-jump-out-args)))

;; text exchange operator
;;(use-package evil-exchange) ; not available in melpa-stable
;;(setq evil-exchange-key (kbd "zx"))
;;(evil-exchange-install)

(unless (display-graphic-p)
  (use-package evil-terminal-cursor-changer))

(use-package evil-visualstar)

;; (use-package evil-jumper) ;; C-i / C-o

;; (use-package evil-surround) ; not available in melpa-stable
;;(global-evil-surround-mode 0)

(use-package evil-states
  :init
  (progn
    (setq evil-emacs-state-cursor '("red" box))
    (setq evil-normal-state-cursor '("green" box))
    (setq evil-visual-state-cursor '("orange" box))
    (setq evil-insert-state-cursor '("red" bar))
    (setq evil-replace-state-cursor '("red" bar))
    (setq evil-operator-state-cursor '("red" hollow))
    ))

;; (define-key global-map [(control ?z) ?u] 'uniq-lines)

(use-package ace-window
  :init
  (progn
    (global-set-key (kbd "M-o") 'ace-window)
    ;; the sequence of leading characters for each window:
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

(use-package projectile
  :init
  (progn
    (projectile-global-mode)))

(use-package auto-complete-config
  :init
  (progn
    (ac-config-default)))

(use-package powerline-evil-themes
  :init
  (progn
    (powerline-evil-center-color-theme)))

 ;; (setq redisplay-dont-pause t
 ;;       scroll-margin 1
 ;;       scroll-step 1
 ;;       scroll-conservatively 10000
 ;;       scroll-preserve-screen-position 1)

 ;; (setq mouse-wheel-follow-mouse 't)
 ;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; another possibility how to define a key chord:
;; (global-set-key [(control x) (?0)] 'sticky-window-delete-window)
(use-package sticky-windows
  :bind (("C-x 0" . sticky-window-delete-window)
         ("C-x 1" . sticky-window-delete-other-windows)
         ("C-x 9" . sticky-window-keep-window-visible)))

;; (global-set-key (kbd "s-i") '(lambda ()
;;                                (interactive)
;;                                (insert "git --git-dir=../credit.git/ ")))

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

(use-package hideshow
  :bind (("C-M-<right>" . hs-show-block)
         ("C-M-<left>"  . hs-hide-block)
         ("C-M-<prior>" . hs-hide-all)
         ("C-M-<next>"  . hs-show-all)
         ((kbd "M-+")   . toggle-hiding)
         ((kbd "s-\\")  . toggle-selective-display))
  :init
  (progn
    ;; (add-hook 'c-mode-common-hook   'hs-minor-mode)
    (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
    (add-hook 'java-mode-hook       'hs-minor-mode)
    (add-hook 'lisp-mode-hook       'hs-minor-mode)
    ;; (add-hook 'perl-mode-hook       'hs-minor-mode)
    (add-hook 'sh-mode-hook         'hs-minor-mode)

    (defun toggle-selective-display (column)
      (interactive "P")
      (set-selective-display
       (or column
           (unless selective-display
             (1+ (current-column))))))

    (defun toggle-hiding (column)
      (interactive "P")
      (if hs-minor-mode
          (if (condition-case nil
                  (hs-toggle-hiding)
                (error t))
              (hs-show-all))
        (toggle-selective-display column)))
    ))

;; (defun display-code-line-counts (ov)
;;   (when (eq 'code (overlay-get ov 'hs))
;;     (overlay-put ov 'help-echo
;;                  (buffer-substring (overlay-start ov)
;;                                    (overlay-end ov)))))

;; (setq hs-set-up-overlay 'display-code-line-counts)

;; Ctrl+Meta+PageUp
;; (global-set-key [C-M-prior] '(lambda ()
;;                                (interactive)
;;                                (hide-body)))
;; Ctrl+Meta+PageDown
;; (global-set-key [C-M-next] '(lambda ()
;;                               (interactive)
;;                               (show-all)))

(use-package whitespace
  :bind (((kbd "s-w") . whitespace-mode)
         ((kbd "s-<f7>") . whitespace-cleanup))
  :init
  (progn
    (setq require-final-newline t)
    (set-default 'indicate-empty-lines t)
    (setq show-trailing-whitespace t)))

;; Always prefer to load newer files, instead of giving precedence to the .elc files
(setq load-prefer-newer t)

(use-package popwin
  :idle (popwin-mode 1))
(defvar popwin:special-display-config-backup popwin:special-display-config)
;; (setq display-buffer-function 'popwin:display-buffer)

;; basic
(push '("*Help*" :stick t :noselect t) popwin:special-display-config)
(push '("*helm world time*" :stick t :noselect t) popwin:special-display-config)

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

;; direx
(push '(direx:direx-mode :position left :width 40 :dedicated t)
      popwin:special-display-config)

(push '("*Occur*" :stick t) popwin:special-display-config)

;; prodigy
(push '("*prodigy*" :stick t) popwin:special-display-config)

;; malabar-mode
(push '("*Malabar Compilation*" :stick t :height 30)
      popwin:special-display-config)

;; org-mode
(push '("*Org tags*" :stick t :height 30)
      popwin:special-display-config)

;; Completions
(push '("*Completions*" :stick t :noselect t) popwin:special-display-config)

;; ggtags
(push '("*ggtags-global*" :stick t :noselect t :height 30) popwin:special-display-config)

;; async shell commands
(push '("*Async Shell Command*" :stick t) popwin:special-display-config)

(use-package evil-nerd-commenter
  :bind ("C-;" . evilnc-comment-or-uncomment-lines))

(use-package color-identifiers-mode
  :init
  (progn
    (add-hook 'after-init-hook 'global-color-identifiers-mode)))

(defun my/enable-color-identifiers ()
  (interactive)
  (color-identifiers-mode t))

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

(global-set-key (kbd "<C-up>") 'xah-backward-block)
(global-set-key (kbd "<C-down>") 'xah-forward-block)


;; I hate smart quotes, too
(defcustom smart-to-ascii '(("\x201C" . "\"")
                            ("\x201D" . "\"")
                            ("\x2018" . "'")
                            ("\x2019" . "'")
                            ;; en-dash
                            ("\x2013" . "-")
                            ;; em-dash
                            ("\x2014" . "-"))
  "Map of smart quotes to their replacements"
  :type '(repeat (cons (string :tag "Smart Character  ")
                       (string :tag "Ascii Replacement"))))

(defun my/smart-to-ascii (beg end)
  "Replace smart quotes and dashes with their ASCII equivalents"
  (interactive "r")
  (format-replace-strings smart-to-ascii
                          nil beg end))
(use-package emacs
  :init
  (progn
    (define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)
    ;; (global-set-key (kbd "M-s") 'save-buffer)
    ;; s-s is here just to have consistent key mapping.
    ;; If it's gonna work I can use M-s for something else
    (global-set-key (kbd "s-s") 'save-buffer)
    (global-set-key (kbd "s-f") 'find-file)
    (global-set-key (kbd "s-c") 'kill-ring-save) ; copy
    (global-set-key (kbd "s-x") 'kill-region)    ; cut
    (global-set-key (kbd "s-v") 'yank)           ; paste
    ;; (global-set-key (kbd "s-b") 'ido-switch-buffer) ; s-b used for helm-mini
    ;; (global-set-key (kbd "s-k") 'ido-kill-buffer)
    (defun close-buffer ()
      (interactive)
      (if server-buffer-clients
          (server-edit)
        (kill-this-buffer)))

    (global-set-key (kbd "s-k") 'close-buffer)
    ;; TODO C-w: close-buffer, /: search-forward in normal mode
    ;; (global-set-key (kbd "<C-w>") 'close-buffer)

    ;; TODO slash: search forward in normal mode
    ;; (global-set-key (kbd "/") 'search-forward)

    (global-set-key (kbd "s-q") 'other-window)   ; (kbd "s-<tab>") does not work
    (global-set-key (kbd "<S-iso-lefttab>") 'other-window)

    (global-set-key (kbd "s-0") 'delete-window)
    (global-set-key (kbd "s-1") 'delete-other-windows)
    (global-set-key (kbd "s-2") 'split-window-below)
    (global-set-key (kbd "s-3") 'split-window-right)

    (global-set-key (kbd "s-h") 'describe-key)

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
     '(paradox-github-token nil) ;; do not be able to star packages
     '(show-paren-mode t)
     '(tab-width 4)
     '(tool-bar-mode nil nil (tool-bar)))

    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    ;; (((( ((( () ))) )))) [[[[ [[[ [] ]]] ]]]] {{{{ {{{ {} }}} }}}}  ; test delimiters:

    ;; check on saving whether the edited file contains a shebang - if yes make it executable
    (add-hook 'after-save-hook
              'executable-make-buffer-file-executable-if-script-p)

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

    (add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

    (defun emacs-lisp-mode-keys ()
      "Modify keymaps used by `emacs-lisp-mode'."
      (local-set-key (kbd "s-e") 'eval-last-sexp))
    (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-keys)

    (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

    ;; (window-configuration-to-register ?w) ;; store / restore : C-x r j / C-x r j w

    ;; (setq tramp-default-method "ssh")
    ))

;; TODO install & use smartparens & paredit
(setq debug-on-error nil)
