;;; cider-startup.el --- startup file for cider. -*- lexical-binding: t -*- 
;;; Code:

(use-package cider :defer t :ensure t
  :config (bind-keys :map cider-repl-mode-map
                     ("s-c" . cider-repl-clear-buffer))
  :bind (
         ("s-z"   . cider-switch-to-repl-buffer)
         ;; ("s-z"   . cider-switch-to-last-clojure-buffer)
         ("s-t"   . cider-test-run-tests)
         ("s-."   . cider-find-var)
         ("s-,"   . cider-jump-back)
         ("C-s-j" . cider-jack-in)
         ("s-r"   . cider-eval-last-expression-in-repl)
         ("s-e"   . cider-eval-last-sexp)
         ("s-l"   . cider-save-and-load-current-buffer)
         ("s-n"   . cider-repl-set-ns)
         ("s-t"   . cider-test-run-tests)
         ("s-."   . cider-find-var)
         ("s-,"   . cider-jump-back)
         ("M-m"   . main-all))
  :init
  (defun main-all ()
    (interactive)
    (end-of-buffer)
    (message "(-main \"-a\")"))

  ;; cider depends on clojure mode
  (use-package clojure-mode :defer t :ensure t
    :init
    (clojure-mode)
    (defun clojure-mode-keys ()
      "Modify keymaps used by `repl-mode'."
      (if (featurep 'evil-leader)
          (evil-leader/set-key "e" 'cider-eval-last-sexp)))
    (add-hook 'clojure-mode-hook 'clojure-mode-keys))

  ;;  kibit - lein plugin for detecting / improving non-idiomatic clj code
  (use-package kibit-helper       :defer t :ensure t)
  (use-package cider-eval-sexp-fu :defer t :ensure t)
  (use-package ac-cider           :defer t :ensure t)
  (use-package rainbow-delimiters :defer t :ensure t)

  (setq nrepl-log-messages t
        nrepl-hide-special-buffers t
        cider-prefer-local-resources t
        ;; cider-auto-select-error-buffer nil
        ;; cider-stacktrace-default-filters '(tooling dup)
        nrepl-buffer-name-separator "-"
        nrepl-buffer-name-show-port t
        cider-repl-display-in-current-window t
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
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

  (defun cider-save-and-load-current-buffer ()
    (interactive)
    (when (buffer-modified-p)
      (save-buffer))
    (cider-load-file (buffer-file-name))
    ;; (cider-switch-to-relevant-repl-buffer nil)
    )

  ;; (defun cider-eval-last-expression-in-repl ()
  ;;   "This doesn't work"
  ;;   (interactive)
  ;;   (evil-visual-char)
  ;;   (evil-jump-item)
  ;;   ;; (clipboard-kill-ring-save)
  ;;   ;; (clipboard-kill-region)
  ;;   ;; (cider-switch-to-relevant-repl-buffer)
  ;;   ;; (clipboard-yank)

  ;;   ;; (global-set-key [(shift delete)] 'clipboard-kill-region)
  ;;   ;; (global-set-key [(control insert)] 'clipboard-kill-ring-save)
  ;;   ;; (global-set-key [(shift insert)] 'clipboard-yank)
  ;;   )

  ;; (defun cider-mode-keys ()
  ;;   "Modify keymaps used by `cider-mode'."
  ;;   (local-set-key (kbd "s-z")
  ;;                  ;; 'cider-switch-to-repl-buffer
  ;;                  'cider-switch-to-last-clojure-buffer)
  ;;   (local-set-key (kbd "s-t") 'cider-test-run-tests)
  ;;   (local-set-key (kbd "s-.") 'cider-find-var)
  ;;   (local-set-key (kbd "s-,") 'cider-jump-back)
  ;;   ;; <menu> key does not work
  ;;   ;; (local-set-key (kbd "<menu>-c") 'cider-repl-clear-buffer)
  ;;   )
  ;; (add-hook 'cider-mode-hook 'cider-mode-keys)

  ;; (defun cider-repl-mode-keys ()
  ;;   "Modify keymaps used by `cider-repl-mode'."
  ;;   (local-set-key (kbd "s-z")
  ;;                  ;; 'cider-switch-to-repl-buffer
  ;;                  'cider-switch-to-last-clojure-buffer)
  ;;   (local-set-key (kbd "s-t") 'cider-test-run-tests)
  ;;   (local-set-key (kbd "s-.") 'cider-find-var)
  ;;   (local-set-key (kbd "s-,") 'cider-jump-back)
  ;;   ;; <menu> key does not work
  ;;   ;; (local-set-key (kbd "<menu>-c") 'cider-repl-clear-buffer)
  ;;   )
  ;; (add-hook 'cider-repl-mode-hook 'cider-repl-mode-keys)

  ;; (defun cider-interaction-mode-keys ()
  ;;   "Modify keymaps used by `cider-interaction-mode'."
  ;;   ;; (local-set-key (kbd "s-o") 'cider-jump)
  ;;   )
  ;; (add-hook 'cider-interaction-mode-hook 'cider-interaction-mode-keys)
  )
(provide 'cider-startup)
