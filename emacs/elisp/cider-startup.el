;;; cider-startup.el --- startup file for cider. -*- lexical-binding: t -*-
;;; Code:

(use-package cider :defer t :ensure t
  ;; TODO cider-repl-mode :diminish "Ç»"
  :diminish " ç"
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

         ("s-M"   . main-all)) ; invoke from *.clj buffer
  :bind (:map cider-mode-map
              ("s-z"   . cider-switch-to-repl-buffer))
  :config
  (setq gui-elements 1) ; because of CIDER menu
  (bind-keys :map cider-repl-mode-map
             ("s-c" . cider-repl-clear-buffer)
             ("s-e" . cider-eval-last-sexp)
             ("s-M" . main-all)) ; invoke from REPL buffer
  :init
  (defun main-all ()
    (interactive)
    (cider-switch-to-repl-buffer)
    (end-of-buffer)
    (insert "(-main \"-a\")"))

  (defun zark-colors ()
    (beginning-of-defun)
    (beginning-of-sexp)
    (end-of-sexp))

  (use-package clj-refactor :defer t :ensure t
    :diminish "" ; "Rλ"
    :bind ("s-r" . cljr-rename-symbol)
    :init
    (add-hook 'clojure-mode-hook
              (lambda ()
                (clj-refactor-mode 1)
                (yas-minor-mode 1) ; for adding require/use/import
                ;; eg. rename files with `C-c C-m rf`.
                (cljr-add-keybindings-with-prefix "C-c C-m"))))

  ;; hide *nrepl-connection* and *nrepl-server* when switching buffers
  ;; (setq nrepl-hide-special-buffers t)

  ;; cider depends on clojure mode
  (use-package clojure-mode :defer t :ensure t
    ;; We're in a majore mode diminishing works only for minor modes
    ;; :diminish "Cλ"
    :init
    ;; (setq prettify-symbols-alist nil)

    (use-package clojure-mode-extra-font-locking :ensure t)
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
