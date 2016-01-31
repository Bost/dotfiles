;;; helm-startup.el --- startup file for helm. -*- lexical-binding: t -*-
;;; Code:

;;; Enable Modes (This is loading nearly everything).
;;
;; see https://github.com/thierryvolpiatto/emacs-tv-config/blob/master/init-helm-thierry.el
(use-package helm-config
  :init
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (helm-adaptive-mode 1)  ; adaptive sorting in all sources
  (helm-push-mark-mode 1)) ; improved version of `push-mark'

(defun helm-git-version ()
  (shell-command-to-string "git log --pretty='format:%H' -1"))

(use-package projectile :ensure t)

(use-package helm :ensure t ; :pin melpa-stable
  :diminish "⎈"
  :config
  (bind-keys :map helm-buffer-map
             ("s-a"     . helm-next-line)
             ("<C-tab>" . helm-next-line-exit-minibuf)
             ("C-`"     . helm-prev-line-exit-minibuf))
  :bind (;; web search for PATTERN with search ENGINE
         ;; ("s-u"  . helm-surfraw)
         ("s-h"     . helm-imenu)
         ("<C-tab>" . helm-buffers-list)
         ("C-`"     . helm-buffers-list)

         ("M-x"     . helm-M-x)
         ("s-p"     . helm-projectile)
         ("s-a"     . helm-buffers-list)
         ("C-x b"   . helm-mini)
         ("s-b"     . helm-mini)
         ;; ("<f9>" . helm-list-elisp-packages-no-fetch)
         ;; ("<f9>" . helm-list-elisp-packages)
         ("M-y"     . helm-show-kill-ring))
  :init ; Code to run before PACKAGE-NAME has been loaded.
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (helm-adaptive-mode 1)  ; adaptive sorting in all sources
  (helm-push-mark-mode 1) ; improved version of `push-mark'
  (defun helm-next-line-exit-minibuf ()
    (interactive)
    (helm-move-line-exit-minibuf 'helm-next-line))

  (defun helm-prev-line-exit-minibuf ()
    (interactive)
    (helm-move-line-exit-minibuf 'helm-previous-line))

  (defun helm-move-line-exit-minibuf (f)
    (funcall f)
    ;; (helm-maybe-exit-minibuffer)
    (helm-exit-minibuffer))

  ;; helm-dictionary: local offline dictionaries
  ;; (use-package helm-dictionary :ensure t :defer t)
  ;; (use-package helm-themes :ensure t :defer t)

  ;; TODO bind `cljr-helm` to a key (I'd suggest C-c r) in Clojure mode
  (use-package cljr-helm          :defer t :ensure t)
  (use-package helm-commandlinefu :defer t :ensure t)
  (use-package helm-ag            :ensure t
    :config (setq helm-ag-base-command
                  "ag --nocolor --nogroup --ignore *~ --ignore-case")
    :bind ("<f3>" . helm-ag))
  (use-package helm-cider-history :defer t :ensure t)
  (use-package macrostep          :defer t :ensure t) ; M-x macrostep-expand
  (use-package helm-descbinds     :defer t :ensure t
    :config (helm-descbinds-mode))
  (use-package helm-ls-git        :ensure t
    :bind (("C-x C-d" . helm-browse-project)
           ("C-<f6>" . helm-ls-git-ls)))

  ;; see helm-surfraw; use google-this as an alternative
  (use-package helm-google :defer t :ensure t ; :pin melpa-stable
    :init
    (use-package google-this :ensure t
      :bind (("s-g" . google-this)
             ;; google auto-complete
             ("s-G" . helm-google-suggest)))

    ;; see ace-jump-buffer
    (use-package helm-flycheck :defer t :ensure t
      :config
      (bind-keys :map flycheck-mode-map
                 ("C-c ! h" . helm-flycheck)))

    (use-package helm-projectile :defer t :ensure t)

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
    (use-package persp-projectile :ensure t
      :bind ("C-s-p" . helm-projectile-ack)
      :config
      ;; (helm-projectile-on)
      (projectile-global-mode)
      ;; (desktop-save-mode 1)
      ;; TODO save perspective
      (use-package perspective :defer t :ensure t
        :config (persp-mode)))

    (use-package helm-swoop :ensure t ; search / narow down
      :bind ("M-i" . helm-swoop))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
                          "^(def\\|.*defconstrainedfn"
                          nil))
        ;; see also "s-h" . helm-imenu; TODO see helm-occur
        (bind-key "C-s-h" 'helm-clojure-headlines clojure-mode-map)))

    (progn
      (defun helm-emacs-lisp-headlines ()
        "Display headlines for the current Emacs Lisp file."
        (interactive)
        (helm-headlines "Emacs Lisp headlines"
                        "helm-emacs-lisp-headlines"
                        "^("                    ; don't display any comments
                        ;; "^(\\|^;* [a-zA-Z]+" ; also display comments
                        nil))
      ;; see also "s-h" . helm-imenu; TODO see helm-occur
      (bind-key "C-s-h" 'helm-emacs-lisp-headlines emacs-lisp-mode-map))

    (defun helm-python-headlines ()
      "Display headlines for the current Python file."
      (interactive)
      (helm-headlines "Python headlines"
                      "helm-python-headlines"
                      "\\(^[[:space:]]*\\(def\\|class\\)\\)\\|^#"
                      nil))))

(provide 'helm-startup)
