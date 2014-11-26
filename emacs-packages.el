; # emacs configuration:
; # mkdir ~/.emacs.d/elpa/transpose-frame && cd ~/.emacs.d/elpa/transpose-frame
; wget http://www.emacswiki.org/emacs-en/download/transpose-frame.el
; echo "(add-to-list \'load-path \"~/.emacs.d/elpa/transpose-frame/\")" >> ~/.emacs
; echo "(require \'transpose-frame)" >> ~/.emacs

; TODO M-x package-list-packages must be called before M-x eval-buffer works
(setq
 package-list-melpa-stable
 '(
   ac-nrepl
   ace-jump-mode
   ack
   ack-and-a-half
   ;; Space align various Clojure forms [github]
   align-cljlet
   anzu
   ;; 'current match/total matches' in the mode-line (pink stuff bottom left)
   apt-utils
   async
   auto-complete
   auto-highlight-symbol
   bind-key
   browse-kill-ring
   ;; Extensions to emacs buffer-selection library (bs.el) [wiki]
   bs-ext
   charmap
   cider
   clj-refactor
   cljdoc
   cljsbuild-mode
   clojure-cheatsheet
   clojure-mode
   clojure-snippets
   clojure-test-mode
   clojurescript-mode
   ;; what is linter good for?
   closure-lint-mode
   closure-template-html-mode
   color-theme
   color-theme-buffer-local
   csv-mode
   dash
   diff-hl
   dircmp
   dired-dups
   dired-single
   duplicate-thing
   ;; running leiningen commands from emacs
   elein
   elisp-slime-nav
   epl
   ergoemacs-mode
   evil
   evil-args
   evil-indent-textobject
   evil-leader
   evil-matchit
   evil-nerd-commenter
   evil-numbers
   evil-org
   evil-paredit
   evil-search-highlight-persist
   evil-visualstar
   expand-region
   f
   ;; probably not needed because of helm
   find-file-in-project
   flycheck
   fold-dwim
   fold-this
   fullscreen-mode
   gh
   gist
   git-commit-mode
   git-rebase-mode
   gitignore-mode
   gitlab
   google
   google-this
   goto-chg
   goto-last-change
   hackernews
   helm
   helm-ack
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
   js2-mode
   keyfreq
   latest-clojars
   levenshtein
   linum-relative
   load-theme-buffer-local
   logito
   magit
   magit-push-remote
   minimap
   mode-icons
   move-text
   multiple-cursors
   neotree
   nrepl
   org
   paredit
   paredit-menu
   pcache
   pkg-info
   popup
   projectile
   queue
   rainbow-delimiters
   request
   s
   simple-httpd
   skewer-mode
   slime
   slime-clj
   slime-repl
   smex
   tidy
   undo-tree
   use-package
   yasnippet
   ))

(setq
 package-list-melpa ;; melpa head; potentialy not stable
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
 package-list-melpa-stable)

;; TODO check how to use require-package
(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))
