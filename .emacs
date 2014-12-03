;; (setq debug-on-error t)

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
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ))

;; activate all the packages (in particular autoloads)
(package-initialize)
;; (package-refresh-contents)

;; 'current match/total matches' in the mode-line (pink stuff bottom left)
(global-anzu-mode +1)

(size-indication-mode 1)  ; filesize indicator

(eval-after-load "paredit.el"
  '(require 'paredit-menu))

(global-set-key (kbd "s-<left>") 'paredit-backward-slurp-sexp)
(global-set-key (kbd "s-<right>") 'paredit-backward-barf-sexp)
;; org-babel-clojure
;; (require 'ob-clojure)
(global-set-key (kbd "s-t") 'clojure-jump-between-tests-and-code)
;; Attention defaults are:
;;     C-c C-l: (cider-load-file FILENAME)
;;     C-c C-k: (cider-load-current-buffer)

(defun emacs-lisp-mode-keys ()
  "Modify keymaps used by `emacs-lisp-mode'."
  (local-set-key (kbd "s-e") 'eval-last-sexp)
  )
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-keys)

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
(setq nrepl-hide-special-buffers t)

(require 'auto-complete-config)
(ac-config-default)

(require 'linum-relative)
(global-linum-mode t)
(global-set-key (kbd "s-n") 'linum-relative-toggle)

;; minibuffer completion incremental feedback
(icomplete-mode 99)  ; turn on icomplete-mode

(defalias 'yes-or-no-p 'y-or-n-p)

;; reload all buffers when the file is changed
(global-auto-revert-mode t)

;; this is for the emacs code browser
(setq stack-trace-on-error t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(csv-separators (quote (";")))
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote ("~/dev/webcli")))
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil nil (tool-bar)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark goldenrod"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "goldenrod"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "light goldenrod"))))
 '(region ((t (:background "#006400")))))

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

;;(desktop-load-default)
;;(desktop-read)

;; (require 'org-install)
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

;; Setup custom shortcuts
;;(global-set-key "\C-x\C-g" 'goto-line)
;;(global-set-key [f1] 'compile)
;;(global-set-key [f2] 'next-error)

;;(add-to-list 'load-path "~/.emacs.d/edit-server/")

(autoload 'magit-status "magit" nil t)
;; (global-set-key [f6] 'split-window-horizontally)
(global-set-key [f6] 'magit-status)

(setq x-select-enable-clipboard-manager nil) ; prevent: Error saving to X clipboard manager.

(load-library "environment-lib")
(set-face-attribute 'default nil :height (get-font-height))

;; highlight current line - this is probably not needed in the default face
;; (global-hl-line-mode 1)
(column-number-mode 1)

;; -t: semicolon is the command line terminator.
;; default is end-of-line as a SQL statement terminator
;; (setq sql-db2-options '("-c" "-i" "-w" "db2" "-tv"))

(setq default-truncate-lines t) ;; no line wrap
(define-key global-map [f5] 'toggle-truncate-lines)

;;(defun cygpath ()
;;  (setq path (thing-at-point 'line))
;;  (split-window-below)
;;  (eshell)
;;  (interactive)
;;  (insert (concat "cygpath -u " path))
;;  (eshell-send-input))
;;(global-set-key [f7] 'cygpath)

(global-set-key [f8] 'transpose-frame)

(add-to-list 'load-path "~/.emacs.d/elpa/neotree")
(require 'neotree)
(global-set-key [s-f8] 'neotree-toggle)

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

;; (global-set-key (kbd "M-s") 'save-buffer)
;; s-s is here just to have consistent key mapping.
;; If it's gonna work I can use M-s for something else
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-f") 'find-file)
(global-set-key (kbd "s-c") 'kill-ring-save) ; copy
(global-set-key (kbd "s-x") 'kill-region)    ; cut
(global-set-key (kbd "s-v") 'yank)           ; paste
(global-set-key (kbd "s-b") 'ido-switch-buffer)
;; (global-set-key (kbd "s-k") 'ido-kill-buffer)

(defun close-buffer ()
  (interactive)
  (if server-buffer-clients
      (server-edit)
    (kill-this-buffer)))

(global-set-key (kbd "s-k") 'close-buffer)

(global-set-key (kbd "s-q") 'other-window)   ; (kbd "s-<tab>") does not work
(global-set-key (kbd "<S-iso-lefttab>") 'other-window)

(global-set-key (kbd "s-0") 'delete-window)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-3") 'split-window-right)

(global-set-key (kbd "s-h") 'describe-key)

;; (define-key global-map [f5] 'toggle-truncate-lines)

;; TODO helm-mode: <tab> should work in minibuffer as without helm
(helm-mode 1)

(require 'move-text)
(move-text-default-bindings)

(defun kill-line-backward (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

;; C-S-backspace is the default key binding for kill-whole-line
(global-set-key (kbd "<C-S-backspace>") 'kill-line-backward)
(global-set-key (kbd "<C-S-delete>") 'kill-line)

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

;; layout management
(winner-mode 1)

(load-library "evil-numbers-lib")
;; or only in evil’s normal state:
;; (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
;; (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; enable global-evil-leader-mode before evil-mode, otherwise
;; evil-leader won’t be enabled in initial buffers (*scratch*, *Messages*, ...)
(global-evil-leader-mode)

(require 'evil)
(evil-mode 1)

(define-key evil-normal-state-map (kbd "<C-O>") 'evil-jump-forward)
(define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)
(global-set-key (kbd "<C-kp-multiply>") 'highlight-symbol-at-point)
;; (global-set-key (kbd "<S-delete>") 'kill-line)
(global-set-key (kbd "<S-delete>") 'kill-region)
;; (global-set-key (kbd "<S-delete>") 'clipboard-kill-region)

(setq evil-leader/in-all-states t)

(evil-leader/set-key
  "wr" 'toggle-truncate-lines
  "dd" 'kill-whole-line
  )

(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)
(evil-leader/set-key "SPC" 'evil-search-highlight-persist-remove-all)

(add-to-list 'load-path "~/.emacs.d/elpa/transpose-frame/")
(require 'transpose-frame)

(setq display-time-24hr-format 1)
(display-time-mode 1)

(defun back-window ()
  ;; opposite of other-window
  (interactive)
  (other-window -1))

;; cycle through buffers with Ctrl-Tab / Shift-Ctrl-Tab
(global-set-key (kbd "<C-tab>") 'bury-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'unbury-buffer)

(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(define-key global-map (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

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

(global-set-key [f9] 'package-list-packages-no-fetch)
(global-set-key (kbd "<s-f9>") 'package-auto-upgrade)

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

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-all-like-this-in-defun)

;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-M->") 'mc/unmark-next-like-this)

(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-<") 'mc/unmark-previous-like-this)

;; (global-set-key (kbd "C->") 'mc/mark-next-word-like-this)
;; (global-set-key (kbd "C-M->") 'mc/unmark-next-word-like-this)

;; (global-set-key (kbd "C-<") 'mc/mark-previous-word-like-this)
;; (global-set-key (kbd "C-M-<") 'mc/unmark-previous-word-like-this)

(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-M-<") 'mc/unmark-all-like-this)

(global-set-key (kbd "<f2>") 'ace-jump-mode)

;; TODO install 'diminish' in order to use 'use-package'
;; Error during download request: Not Implemented

;; (require 'use-package)
;; (use-package ace-jump-mode
;;   :bind ("<f2>" . ace-jump-mode))

;; (global-set-key (kbd "<f3>") 'whitespace-mode) ;; f3 / C-x ( and f4 / C-x ) are for macros
;; (global-unset-key (kbd "<f3>"))
;; (global-set-key (kbd "<f3>") 'kmacro-start-macro)

;; (require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;; yasnippets does not to work
;; (add-to-list 'load-path
;;           "~/.emacs.d/elpa/clojure-snippets-20130403.2046/snippets/clojure-mode")
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; (define-key yas-minor-mode-map (kbd "s-y") 'yas/expand)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program
      ;; "chromium-browser" does not work properly on ubuntu 13.10
      ;; "chrome" ; cygwin
      "google-chrome")

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; xfce4-settings-manager -> Window Manger -> Keyboard -> ...
(require 'duplicate-thing)
(global-set-key [C-M-up] 'duplicate-thing)
(global-set-key [C-M-down] 'duplicate-thing)

(defun ignore-error-wrapper (fn)
  "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))

(global-set-key [C-s-left] (ignore-error-wrapper 'windmove-left))
(global-set-key [C-s-right] (ignore-error-wrapper 'windmove-right))
(global-set-key [C-s-up] (ignore-error-wrapper 'windmove-up))
(global-set-key [C-s-down] (ignore-error-wrapper 'windmove-down))

(global-set-key [M-s-left] 'shrink-window-horizontally)
(global-set-key [M-s-right] 'enlarge-window-horizontally)
(global-set-key [M-s-down] 'enlarge-window)
(global-set-key [M-s-up] 'shrink-window)

(global-set-key (kbd "s-m") 'minimap-toggle)

;; (load-library "abbrev-table")
;;(global-set-key [f11] 'abbrev-mode)
;;(global-set-key [f11] 'toggle-frame-fullscreen) ; this is the default
(toggle-frame-maximized)

(global-set-key [f7] 'find-file-emacs)
(global-set-key (kbd "s-<f11>") 'find-file-emacs)


(global-set-key [f12] 'undo-tree-visualize)
;; (global-set-key (kbd "s-<f7>") 'whitespace-cleanup)
(global-set-key (kbd "s-<f7>") 'whitespace-mode)
(global-set-key (kbd "s-w") 'whitespace-mode)
;; (global-set-key [scroll] 'exec-test-macro)

(defun switch-to-buffer-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(global-set-key (kbd "<s-f12>") 'switch-to-buffer-scratch)

;; TODO use `add-hook' to add `rainbow-delimiters-mode' to the hooks
;; of the major modes you actually want to use `rainbow-delimiters' in.
;; (require 'rainbow-delimiters)
;; To enable it only in certain modes, add lines like the following:
;; (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;; To enable it in all programming-related emacs modes (Emacs 24+):
;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; To activate the mode globally, add to your init file:
;; (global-rainbow-delimiters-mode)
;; (((( ((( () ))) )))) [[[[ [[[ [] ]]] ]]]] {{{{ {{{ {} }}} }}}}  ; test delimiters:

;; must be loaded in the end; otherwise:
;;    Symbol's function definition is void: mapcar*
;; (require 'workgroups2)

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
(require 'evil-args)

;; bind evil-args text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; bind evil-forward/backward-args
(define-key evil-normal-state-map "L" 'evil-forward-arg)
(define-key evil-normal-state-map "H" 'evil-backward-arg)
(define-key evil-motion-state-map "L" 'evil-forward-arg)
(define-key evil-motion-state-map "H" 'evil-backward-arg)

;; bind evil-jump-out-args
(define-key evil-normal-state-map "K" 'evil-jump-out-args)


;; text exchange operator
;;(require 'evil-exchange) ; not available in melpa-stable
;;(setq evil-exchange-key (kbd "zx"))
;;(evil-exchange-install)

(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer))

(require 'evil-visualstar)

;; TODO (load "~/bin/dbases.el")
;; (if (> (string-to-number (getenv "isLinuxVB")) 0)
;;     (load "~/bin/dbases.el"))

;; (require 'evil-jumper) ;; C-i / C-o

;; (require 'evil-surround) ; not available in melpa-stable
;;(global-evil-surround-mode 0)

(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

;; (define-key global-map [(control ?z) ?u] 'uniq-lines)

(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(projectile-global-mode)


;; avoid warning while emacs-24.4 start up
;; (add-to-list 'load-path "~/.emacs.d")

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)

(powerline-evil-center-color-theme)

;; (setq redisplay-dont-pause t
;;       scroll-margin 1
;;       scroll-step 1
;;       scroll-conservatively 10000
;;       scroll-preserve-screen-position 1)

;; (setq mouse-wheel-follow-mouse 't)
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(require 'sticky-windows)
(global-set-key [(control x) (?0)] 'sticky-window-delete-window)
(global-set-key [(control x) (?1)] 'sticky-window-delete-other-windows)
(global-set-key [(control x) (?9)] 'sticky-window-keep-window-visible)

(global-set-key (kbd "s-p") 'evil-mode)
(global-set-key (kbd "s-i") '(lambda ()
                               (interactive)
                               (insert "git --git-dir=../credit.git/ ")))
