; # emacs configuration:
; # mkdir ~/.emacs.d/elpa/transpose-frame && cd ~/.emacs.d/elpa/transpose-frame
; wget http://www.emacswiki.org/emacs-en/download/transpose-frame.el
; echo "(add-to-list \'load-path \"~/.emacs.d/elpa/transpose-frame/\")" >> ~/.emacs
; echo "(require \'transpose-frame)" >> ~/.emacs

; TODO M-x package-list-packages must be called before M-x eval-buffer works
(setq
 package-list 
 '(
   ac-helm
   ac-nrepl
   ace-jump-buffer
   ace-jump-mode
   achievements
   ack
   ack-and-a-half
   ack-menu
   align-cljlet        ; Space align various Clojure forms [github]
   anzu
   apt-utils
   auto-complete
   auto-complete-nxml
   auto-dim-other-buffers
   auto-highlight-symbol
   auto-save-buffers-enhanced
   bf-mode             ; Browse file persistently on dired [github]
   browse-kill-ring
   bs-ext              ; Extensions to emacs buffer-selection library (bs.el) [wiki]
   btc-ticker
   charmap
   cider
   clj-refactor
   cljdoc
   cljsbuild-mode
   clojure-cheatsheet
   clojure-mode
   clojure-project-mode
   clojure-snippets
   clojure-test-mode
   clojurescript-mode
   closure-lint-mode   ; what is linter good for?
   closure-template-html-mode
   color-theme
   color-theme-buffer-local
   csv-mode
   csv-nav
   dash
   diff-hl
   dircmp
   dired+
   dired-dups
   dired-single
   dired-toggle
   dirtree
   elein
   elisp-slime-nav
   epl
   ergoemacs-mode
   evil
   evil-args
   evil-exchange
   evil-indent-textobject
   evil-leader
   evil-matchit
   evil-nerd-commenter
   evil-numbers
   evil-paredit
   evil-terminal-cursor-changer
   evil-visualstar
   expand-region
   f
   find-file-in-project         ; probably not needed because of helm
   flycheck
   fold-dwim
   fold-this
   fullscreen-mode
   gh
   gist
   git-commit-mode
   git-rebase-mode
   gitignore-mode
   google-maps
   google-this
   google-weather
   goto-chg
   goto-last-change
   hackernews
   haskell-mode
   helm
   helm-ack
   helm-cmd-t
   helm-dired-recent-dirs
   helm-gist
   helm-git
   helm-helm-commands
   helm-ls-git
   helm-spaces
   helm-themes
   highlight-symbol
   idle-highlight-mode
   ido-ubiquitous
   js2-mode
   keyfreq
   latest-clojars
   levenshtein
   linum-relative
   litable
   load-theme-buffer-local
   logito
   mag-menu
   magit
   magit-commit-training-wheels
   magit-push-remote             ; probably not needed
   ;; magithub - magit-log-edit-confirm-cancellation issue
   mode-icons
   move-text
   multiple-cursors
   nrepl
   org
   paredit
   paredit-menu
   pcache
   pkg-info
   popup
   project-mode
   rainbow-delimiters
   redo+
   request
   s
   simple-httpd
   skewer-mode
   slime
   slime-clj
   slime-repl
   smex
   spaces
   splitter
   tidy
   tree-mode
   undo-tree
   windata
   workgroups2
   yasnippet
   ))

(package-initialize)
;; check if the packages is installed; if not, install it.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package)) 
           (package-install package))))
 ;; '(dired+ magit rainbow-mode)
 package-list)
