; # emacs configuration:
; # mkdir ~/.emacs.d/elpa/transpose-frame && cd ~/.emacs.d/elpa/transpose-frame
; wget http://www.emacswiki.org/emacs-en/download/transpose-frame.el
; echo "(add-to-list \'load-path \"~/.emacs.d/elpa/transpose-frame/\")" >> ~/.emacs
; echo "(require \'transpose-frame)" >> ~/.emacs

(setq
 package-list 
 '(
   ac-helm
   ac-nrepl
   ace-jump-buffer
   ace-jump-mode
   ack
   ack-and-a-half
   ack-menu
   align-cljlet        ; Space align various Clojure forms [github]
   auto-complete
   auto-complete-nxml
   bf-mode             ; Browse file persistently on dired [github]
   browse-kill-ring
   bs-ext              ; Extensions to emacs buffer-selection library (bs.el) [wiki]
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
   csv-mode
   csv-nav
   dash
   dircmp
   dired+
   elisp-slime-nav
   evil
   evil-indent-textobject
   evil-leader
   evil-nerd-commenter
   evil-numbers
   evil-paredit
   evil-visualstar
   expand-region
   ;; find-file-in-project - probably not needed because of helm
   gist
   gitignore-mode
   git-commit-mode
   git-rebase-mode
   google-maps
   google-this
   google-weather
   hackernews
   helm
   helm-ack
   helm-helm-commands
   helm-spaces
   highlight-symbol
   idle-highlight-mode
   ido-ubiquitous
   js2-mode
   latest-clojars
   latest-clojure-libraries
   levenshtein
   linum-relative
   litable
   load-theme-buffer-local
   mag-menu
   magit
   magit-log-edit
   magit-commit-training-wheels
   ;; magit-push-remote - probably not needed - test it
   ;; magithub - magit-log-edit-confirm-cancellation issue
   mode-icons
   move-text
   multiple-cursors
   nrepl
   org
   paredit
   paredit-menu
   pkg-info
   popup
   ;; project - probably too old
   project-mode
   rainbow-delimiters
   redo+
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
   undo-tree
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
