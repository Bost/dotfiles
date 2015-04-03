;; # emacs configuration:
;; # mkdir ~/.emacs.d/elpa/transpose-frame && cd ~/.emacs.d/elpa/transpose-frame
;; wget http://www.emacswiki.org/emacs-en/download/transpose-frame.el
;; echo "(add-to-list \'load-path \"~/.emacs.d/elpa/transpose-frame/\")" >> ~/.emacs
;; echo "(require \'transpose-frame)" >> ~/.emacs

;; TODO M-x package-list-packages must be called before M-x eval-buffer works

;; C-h v package-activated-list
(setq
 ;; preferably melpa-stable
 package-list-needed
 '(
   ac-cider ; auto-complete for cider
   ac-helm
   ac-nrepl
   ace-jump-buffer
   ace-jump-mode
   ace-window
   ack
   ack-and-a-half
   align-cljlet ; Space align various Clojure forms [github]
   anzu
   apt-utils ; 'current match/total matches' in the mode-line (pink stuff bottom left)
   async
   auto-complete
   auto-complete-nxml ;; TODO compare auto-complete and company-mode (supported by cider): ;; https://github.com/company-mode/company-mode/issues/68
   auto-highlight-symbol
   auto-package-update
   bind-key
   browse-kill-ring
   bs-ext ; Extensions to emacs buffer-selection library (bs.el) [wiki]
   charmap
   cider
   clj-refactor
   cljdoc
   cljr-helm ; wraps clojure refactor command with helm
   cljsbuild-mode
   clojure-cheatsheet
   clojure-mode
   clojure-snippets
   clojurescript-mode
   closure-lint-mode ; what is linter good for?
   closure-template-html-mode
   color-identifiers-mode
   color-theme
   color-theme-buffer-local
   company ; modular completition mechanism ;; TODO compare auto-complete and company-mode (supported by cider): ;; https://github.com/company-mode/company-mode/issues/68
   csv-mode
   csv-nav
   dash
   debbugs
   diff-hl
   diminish
   dircmp
   dired+
   dired-dups
   dired-single
   direx ; TODO test direx: simple directory explorer
   drag-stuff ; replacement for move-lines
   duplicate-thing
   egg ; TODO test egg: Emacs Got Git
   elein ; TODO emacs + leiningen: lein/elein might be obsolete
   elisp-slime-nav
   epl
   ergoemacs-mode
   evil
   evil-anzu
   evil-args
   evil-escape
   evil-iedit-state
   evil-indent-textobject
   evil-leader
   evil-matchit
   evil-nerd-commenter
   evil-numbers
   evil-org
   evil-paredit
   evil-search-highlight-persist
   evil-smartparens
   evil-snipe
   evil-surround
   evil-visualstar
   expand-region
   f
   find-file-in-project ; probably not needed because of helm
   flycheck
   fold-dwim
   fold-this
   fringe-helper
   fullscreen-mode
   gh
   gist
   git-commit-mode
   git-rebase-mode
   gitignore-mode
   gitlab
   google
   google-maps
   google-this
   goto-chg
   goto-last-change
   hackernews
   helm
   helm-ack
   helm-ag
   helm-flycheck
   helm-git
   helm-git-files
   helm-git-grep
   helm-github-stars
   helm-gitlab
   helm-google
   helm-helm-commands
   helm-ls-git
   helm-mode-manager
   helm-open-github
   helm-package
   helm-proc
   helm-projectile
   helm-themes
   highlight
   highlight-symbol
   idle-highlight-mode
   ido-ubiquitous
   iedit
   js2-mode
   keyfreq
   keyfreq
   latest-clojars
   latest-clojure-libraries
   levenshtein
   linum-relative
   load-theme-buffer-local
   logito
   magit
   magit-push-remote
   malabar-mode ; A better Java mode for Emacs
   minimap
   mode-icons
   move-text
   multiple-cursors
   neotree
   org
   paradox
   paredit
   paredit-menu
   pcache
   pkg-info
   popup
   popwin
   powerline
   powerline-evil
   projectile
   queue
   rainbow-delimiters
   rainbow-identifiers
   request
   s
   simple-httpd
   skewer-mode
   slime
   slime-clj
   slime-repl
   smart-mode-line
   smartparens
   smex
   smooth-scrolling ; only smooth-scrolling together with sublimity leads to smooth scrolling really working! WTF?
   sticky-windows
   string-inflection ; underscore -> UPCASE -> CamelCase -> lowerCamelCase conversion of names
   sublimity
   tidy
   undo-tree
   use-package
   window-purpose ; purpose base window management
   yasnippet
   ))

(setq
;; mostly melpa head; potentialy not stable
 package-list-optional
 '(
   dired+
   ;; helm dependency unresolved
   ac-helm
   ace-jump-buffer
   achievements
   ack-menu
   auto-complete-nxml
   auto-dim-other-buffers
   auto-save-buffers-enhanced
   ;; Browse file persistently on dired [github]
   bf-mode
   btc-ticker
   csv-nav
   dired-toggle
   dirtree
   evil-exchange
   evil-surround
   evil-terminal-cursor-changer
   google-maps
   helm-cmd-t
   helm-dired-recent-dirs
   helm-gist
   helm-git
   helm-ls-git
   ;; spaces-0.1.0 not available
   helm-spaces
   ;; minibuffer completion incremental feedback
   icomplete+
   litable
   mag-menuxs
   redo+
   spaces
   splitter
   tree-mode
   windata
   workgroups2
   ))

;; following packages does not need to be installed on every computer
(setq
 package-list-development
 '(
   haskell-mode
   ))

(package-initialize)
;; check if the packages is installed; if not, install it.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))))
 ;; '(dired+ magit rainbow-mode)
 package-list-needed)

;; TODO check how to use require-package
(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))
