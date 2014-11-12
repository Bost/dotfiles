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
   align-cljlet        ; Space align various Clojure forms [github]
   anzu                ; 'current match/total matches' in the mode-line (pink stuff bottom left)
   apt-utils
   async
   auto-complete
   auto-highlight-symbol
   browse-kill-ring
   bs-ext              ; Extensions to emacs buffer-selection library (bs.el) [wiki]
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
   closure-lint-mode   ; what is linter good for?
   closure-template-html-mode
   color-theme
   color-theme-buffer-local
   csv-mode
   dash
   diff-hl
   dircmp
   dired-dups
   dired-single
   elein               ; running leiningen commands from emacs
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
   google-this
   goto-chg
   goto-last-change
   hackernews
   helm
   helm-ack
   helm-helm-commands
   helm-package
   helm-projectile
   helm-themes
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
   magit-push-remote             ; probably not needed
   ;; magithub - magit-log-edit-confirm-cancellation issue
   minimap
   mode-icons
   move-text
   multiple-cursors
   neotree
   nrepl
   paredit
   paredit-menu
   pcache
   pkg-info
   popup
   projectile
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
   yasnippet
   ))

(setq
 package-list-melpa ;; melpa head; potentialy not stable
 '(
   dired+
   ac-helm                       ; helm dependency unresolved
   ace-jump-buffer
   achievements
   ack-menu
   auto-complete-nxml
   auto-dim-other-buffers
   auto-save-buffers-enhanced
   bf-mode                       ; Browse file persistently on dired [github]
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
   helm-spaces                   ; spaces-0.1.0 not available
   icomplete+                    ; minibuffer completion incremental feedback
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
