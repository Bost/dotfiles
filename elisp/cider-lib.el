(defun cider-save-and-load-current-buffer ()
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (cider-load-file (buffer-file-name))
  ;; (cider-switch-to-relevant-repl-buffer nil)
  )

(defun cider-eval-last-expression-in-repl ()
  "This doesn't work"
  (interactive)
  (evil-visual-char)
  (evil-jump-item)
  ;; (clipboard-kill-ring-save)
  ;; (clipboard-kill-region)
  ;; (cider-switch-to-relevant-repl-buffer)
  ;; (clipboard-yank)

  ;; (global-set-key [(shift delete)] 'clipboard-kill-region)
  ;; (global-set-key [(control insert)] 'clipboard-kill-ring-save)
  ;; (global-set-key [(shift insert)] 'clipboard-yank)
  )

(defun clojure-mode-keys ()
  "Modify keymaps used by `clojure-mode'."
  (local-set-key (kbd "s-r") 'cider-eval-last-expression-in-repl)
  (local-set-key (kbd "s-e") 'cider-eval-last-sexp)
  (local-set-key (kbd "s-z") 'cider-switch-to-relevant-repl-buffer)
  (local-set-key (kbd "s-l") 'cider-save-and-load-current-buffer)
  (local-set-key (kbd "s-n") 'cider-repl-set-ns)
  (local-set-key (kbd "s-t") 'cider-test-run-tests)
  (local-set-key (kbd "s-.") 'cider-jump)
  (local-set-key (kbd "s-,") 'cider-jump-back)
  )
(add-hook 'clojure-mode-hook 'clojure-mode-keys)

(defun cider-mode-keys ()
  "Modify keymaps used by `cider-mode'."
  (local-set-key (kbd "s-z") 'cider-switch-to-last-clojure-buffer)
  (local-set-key (kbd "s-.") 'cider-jump)
  (local-set-key (kbd "s-,") 'cider-jump-back)
  )
(add-hook 'cider-mode-hook 'cider-mode-keys)

;; enable eldoc mode in clojure buffers
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; enable paredit in repl buffer
;; (add-hook 'cider-repl-mode-hook 'paredit-mode)

;; smartparens is an alternative to paredit
;; (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)

(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; camel case
;; (add-hook 'cider-repl-mode-hook 'subword-mode)


;; (global-set-key (kbd "s-.") 'cider-jump)
;; (global-set-key (kbd "s-,") 'cider-jump-back)

(defun cider-interaction-mode-keys ()
  "Modify keymaps used by `cider-interaction-mode'."
  ;; (local-set-key (kbd "s-o") 'cider-jump)
  )
(add-hook 'cider-interaction-mode-hook 'cider-interaction-mode-keys)


