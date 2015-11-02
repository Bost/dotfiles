;;; helm-startup.el --- startup file for helm. -*- lexical-binding: t -*-
;;; Code:

;;; Enable Modes (This is loading nearly everything).
;;
;; see https://github.com/thierryvolpiatto/emacs-tv-config/blob/master/init-helm-thierry.el
(use-package helm-config
  :config ; Code to run after PACKAGE-NAME has been loaded
  (progn
    (helm-mode 1)
    (helm-autoresize-mode 1)
    (helm-adaptive-mode 1)  ; adaptive sorting in all sources
    (helm-push-mark-mode 1) ; improved version of `push-mark'
    ))

(defun helm-git-version ()
  (shell-command-to-string
   "git log --pretty='format:%H' -1"))

(use-package helm
  :defer t
  :ensure t
  ;; :pin melpa-stable
  :init ; Code to run before PACKAGE-NAME has been loaded.

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

  ;; see helm-surfraw
  (use-package helm-google ; use google-this as an alternative
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

  ;; (helm :sources (helm-build-sync-source "test"
  ;;                  :candidates '(foo foa fob bar baz)
  ;;                  :fuzzy-match t)
  ;;       :buffer "*helm test*")

  ;; (defun helm-clojure-headlines ()
  ;;   "Display headlines for the current Clojure file."
  ;;   (interactive)
  ;;   (helm-mode t)
  ;;   (helm :sources '(((name . "Clojure Headlines")
  ;;                     (volatile)
  ;;                     (headline "^[;(]")))))

  ;; (defun helm-clojure-headlines ()
  ;;   "Display headlines for the current Clojure file."
  ;;   (interactive)
  ;;   (setq helm-current-buffer (current-buffer)) ;; Fixes bug where the current buffer sometimes isn't used
  ;;   (jit-lock-fontify-now) ;; https://groups.google.com/forum/#!topic/emacs-helm/YwqsyRRHjY4
  ;;   (helm :sources (helm-build-in-buffer-source "Clojure Headlines"
  ;;                    :data (with-helm-current-buffer
  ;;                            (goto-char (point-min))
  ;;                            (cl-loop while (re-search-forward "^(\\|testing\\|^;.*[a-zA-Z]+" nil t)
  ;;                                     for line = (buffer-substring (point-at-bol) (point-at-eol))
  ;;                                     for pos = (line-number-at-pos)
  ;;                                     collect (propertize line 'helm-realvalue pos)))
  ;;                    :get-line 'buffer-substring
  ;;                    :action (lambda (c) (helm-goto-line c)))
  ;;         :buffer "helm-clojure-headlines"))

  ;; see https://gist.github.com/tjg/4903f00a62e02bbe6217
  (defun helm-headlines (headline buffer-name good-regex exception-regex)
    "Display headlines for the current file.
   Displays lines where good-regex matches, except for those
   which also match exception-regex."
    ;; Fixes bug where the current buffer sometimes isn't used
    (setq helm-current-buffer (current-buffer))

    ;; https://groups.google.com/forum/#!topic/emacs-helm/YwqsyRRHjY4
    (jit-lock-fontify-now)
    (let* ((line-count 0)
           (data (with-helm-current-buffer
                   (goto-char (point-min))
                   (cl-loop while (re-search-forward good-regex nil t)
                            for line = (buffer-substring (point-at-bol)
                                                         (point-at-eol))
                            for pos = (line-number-at-pos)
                            unless (and exception-regex
                                        (string-match-p exception-regex line))
                            collect (propertize line 'helm-realvalue pos)
                            and do (incf line-count))))
           (headline (if (< helm-candidate-number-limit line-count)
                         (format "%s (initially showing only %s lines; %s is %s)"
                                 headline
                                 line-count
                                 'helm-candidate-number-limit
                                 helm-candidate-number-limit)
                       headline)))
      (helm :sources (helm-build-in-buffer-source headline
                       :data data
                       :get-line 'buffer-substring
                       :action (lambda (c) (helm-goto-line c)))
            :buffer "*helm M-x*"
            ;; buffer-name
            ;; "*helm mini*"

            ;; :ff-transformer-show-only-basename nil
            :truncate-lines helm-buffers-truncate-lines)))

  ;; TODO consider using :if from use-package
  (when (and (featurep 'helm) (featurep 'clojure-mode))
    (progn
      (defun helm-clojure-headlines ()
        "Display headlines for the current Clojure file."
        (interactive)
        (helm-headlines "Clojure headlines"
                        "helm-clojure-headlines"
                        "^(def"
                        nil))
      ;; TODO see helm-occur
      (bind-key "s-h" 'helm-clojure-headlines clojure-mode-map)))

  (progn
    (defun helm-emacs-lisp-headlines ()
      "Display headlines for the current Emacs Lisp file."
      (interactive)
      (helm-headlines "Emacs Lisp headlines"
                      "helm-emacs-lisp-headlines"
                      "^(\\|^;* [a-zA-Z]+"
                      nil))
    ;; TODO see helm-occur
    (bind-key "s-h" 'helm-emacs-lisp-headlines emacs-lisp-mode-map))

    (defun helm-python-headlines ()
      "Display headlines for the current Python file."
      (interactive)
      (helm-headlines "Python headlines"
                      "helm-python-headlines"
                      "\\(^[[:space:]]*\\(def\\|class\\)\\)\\|^#"
                      nil)))

(provide 'helm-startup)
