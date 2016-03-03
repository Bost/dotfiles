;;; cider-startup.el --- startup file for cider. -*- lexical-binding: t -*-
;;; Code:

(use-package cider :ensure t
  ;; :diminish "C♻" ; works only for minor not major modes
  :bind (;; ("s-z"   . cider-switch-to-repl-buffer)
         ;; ("s-z"   . cider-switch-to-last-clojure-buffer)
         ("s-t"   . cider-test-run-tests)
         ("s-."   . cider-find-var)
         ("s-,"   . cider-jump-back)
         ("C-s-j" . cider-jack-in)
         ;; ("s-r"   . cider-eval-last-expression-in-repl)
         ("s-l"   . cider-save-and-load-current-buffer)
         ("s-n"   . cider-repl-set-ns)
         ("s-t"   . cider-test-run-tests)
         ("s-."   . cider-find-var)
         ("s-,"   . cider-jump-back)
         ;; TODO s-M does not work in REPL buffer
         ("s-o"   . cider-clear-compilation-highlights)

         ;; BUG: "<s-kp-insert>" "<C-insert>" are the same keys Uhg?

         ("<s-kp-insert>" . zark-symbols)
         ("<s-kp-0>"      . zark-symbols)
         ("s-'"           . zark-symbols)
         ;; (unbind-key "<C-insert>")
         ;; ("<C-insert>"    . typed-unicode-symbols)

         ;; invoke from *.clj buffer
         ("C-s-l" . clojure-insert-let)
         ("M-s-p" . clojure-insert-println)
         ("s-M" . main-a)
         ("s-A" . main-a)
         ("s-S" . main-s)
         ("s-U" . main-u)
         ("s-_" . clojure-ignore-next-form)
         ;; on the german keyboard the '#' is next to Enter
         ;; TODO move cursor using
         ;; paredit-copy-as-kill / paredit-backward / paredit-backward-up(down)
         ("s-\\" . clojure-ignore-next-form))
  :config
  (bind-keys :map cider-mode-map
             ("s-z" . cider-switch-to-repl-buffer)
             ("s-e" . cider-eval-last-sexp))
  (setq gui-elements 1) ; because of CIDER menu
  :init
  (use-package cider-repl
    :config
    (bind-keys :map cider-repl-mode-map
               ("s-c" . cider-repl-clear-buffer)
               ("s-e" . cider-eval-last-sexp)
               ("s-z" . cider-switch-to-last-clojure-buffer)
               ;; invoke from *.clj buffer
               ("s-M" . main-a)
               ("s-S" . main-s)))

  (defun main-x (x)
    (cider-switch-to-repl-buffer)
    (end-of-buffer)
    (insert (concat "(-main \"-" x "\")"))
    (evil-insert-state))

  (defun clojure-ignore-next-form ()
    (interactive)
    (insert "#_"))

  (defun clojure-insert-println ()
    (interactive)
    (insert "(println \"\")")
    (left-char 2))

  (defun clojure-insert-let ()
    (interactive)
    (insert "(let [ ])")
    (left-char 3))

  (defun main-a ()
    (interactive)
    (main-x "a"))

  (defun main-s ()
    (interactive)
    (main-x "s"))

  (defun main-u ()
    (interactive)
    (main-x "u"))

  ;; TODO <f2> can jump straight to the desired char; t/f should do the same
  ;; TODO C-c M-f: cider-jack-in + figwheel
  ;; TODO see cider-jack-in-clojurescript
  (defun figwheel-cider ()
    (interactive)
    (cider-interactive-eval "(use 'figwheel-sidecar.repl-api)")
    (cider-interactive-eval "(start-figwheel!)")
    (cider-interactive-eval "(cljs-repl)")
    ;; TODO (rename-buffer "*figwheel-cider*")
    (if (not (evil-insert-state-p))
        (evil-insert 0)))

  (defun figwheel-restart ()
    (interactive)
    (cider-restart t) ; t - RESTART-ALL
    (figwheel-cider))

  (defun zark-colors ()
    (beginning-of-defun)
    (beginning-of-sexp)
    (end-of-sexp))

  ;; TODO eval discover-clj-refactor
  ;; TODO clj-helm depends on clj-refactor
  (use-package clj-refactor
    :ensure t
    ;; :pin melpa-stable ; version 2.0.0; see ~/.lein/profiles.clj
    ;; but cljr-helm needs clj-refactor-0.13.
    :diminish "" ; "Rλ"
    :bind ("s-r" . cljr-rename-symbol)
    :init
    ;; hide *nrepl-connection* and *nrepl-server* when switching buffers
    ;; (setq nrepl-hide-special-buffers t)

    ;; cider depends on clojure mode
    (use-package clojure-mode :defer t :ensure t
      ;; :load-path "~/dev/clojure-mode"
      ;; :diminish "Cλ" ; works only for minor not major modes
      :init
      (use-package typed-clojure-mode :defer t :ensure t)
      ;; (setq prettify-symbols-alist nil)
      (clojure-mode)
      (defun clojure-mode-keys ()
        "Modify keymaps used by `repl-mode'."
        (if (featurep 'evil-leader)
            (evil-leader/set-key "e" 'cider-eval-last-sexp)))
      (add-hook 'clojure-mode-hook 'clojure-mode-keys))

    (add-hook 'clojure-mode-hook
              (lambda ()
                (clj-refactor-mode 1)

                (use-package evil-smartparens :ensure t
                  :config
                  ;; evil-smartparens everywhere
                  ;; (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
                  ;; evil-smartparens only in clojure
                  ;; (sp-pair "\{" "\}")
                  (add-hook 'clojure-mode-hook #'evil-smartparens-mode))

                (use-package clojure-mode-extra-font-locking :ensure t)

                (yas-minor-mode 1) ; for adding require/use/import
                ;; eg. rename files with `C-c C-m rf`.
                (cljr-add-keybindings-with-prefix "C-c C-m"))))

  ;;  kibit - lein plugin for detecting / improving non-idiomatic clj code
  (use-package kibit-helper       :defer t :ensure t)
  (use-package cider-eval-sexp-fu :defer t :ensure t)
  (use-package ac-cider           :defer t :ensure t)

  (setq nrepl-log-messages t
        nrepl-hide-special-buffers t
        cider-prefer-local-resources t
        ;; cider-auto-select-error-buffer nil
        ;; cider-stacktrace-default-filters '(tooling dup)
        nrepl-buffer-name-separator "-"
        nrepl-buffer-name-show-port t
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

  (defun cider-save-and-load-current-buffer ()
    (interactive)
    (when (buffer-modified-p)
      (save-buffer))
    (cider-load-file (buffer-file-name))
    ;; (cider-switch-to-relevant-repl-buffer nil)
    )
  )

(provide 'cider-startup)
