;;; cider-startup.el --- startup file for cider. -*- lexical-binding: t -*-
;;; Code:

(use-package cider :defer t :ensure t
  :bind (("s-z"   . cider-switch-to-repl-buffer)
         ;; ("s-z"   . cider-switch-to-last-clojure-buffer)
         ("s-t"   . cider-test-run-tests)
         ("s-."   . cider-find-var)
         ("s-,"   . cider-jump-back)
         ("C-s-j" . cider-jack-in)
         ("s-r"   . cider-eval-last-expression-in-repl)
         ("s-l"   . cider-save-and-load-current-buffer)
         ("s-n"   . cider-repl-set-ns)
         ("s-t"   . cider-test-run-tests)
         ("s-."   . cider-find-var)
         ("s-,"   . cider-jump-back)
         ;; TODO s-M does not work in REPL buffer
         ("s-o"   . cider-clear-compilation-highlights)

         ;; BUG: "<s-kp-insert>" "<C-insert>" are the same keys Uhg?

         ("<s-kp-insert>" . typed-unicode-symbols)
         ("<s-kp-0>"      . typed-unicode-symbols)
         ;; (unbind-key "<C-insert>")
         ;; ("<C-insert>"    . typed-unicode-symbols)

         ("s-M"   . main-all))
  :config
  (setq gui-elements 1) ; because of CIDER menu
  (bind-keys :map cider-repl-mode-map
             ("s-c" . cider-repl-clear-buffer)
             ("s-e" . cider-eval-last-sexp)
             )
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

  ;; cider depends on clojure mode
  (use-package clojure-mode :defer t :ensure t
    :config
  ;; (defconst lisp--prettify-symbols-alist
  ;;   '((">="  . ?≥)
  ;;     ("defun "  . ?​) ; Unicode Char 'ZERO WIDTH SPACE' (U+200B)
  ;;     ("defun"  . ?φ)
      ;; (push '("t/loop" . ?#x27F3) prettify-symbols-alist)
  ;;     ("lambda"  . ?λ)))

    (setq is-pretty 1)
    (defun typed-unicode-symbols ()
      (interactive)
      (setq prettify-symbols-alist nil)
      (push '("->" . ?→) prettify-symbols-alist)

      (push '("Move" . ?Ѻ) prettify-symbols-alist)
      (push '("PlayerName" . ?⛹) prettify-symbols-alist)
      (push '("PlayerMove" . ?🚶) prettify-symbols-alist)
      (push '("RPSResult" . ?☻) prettify-symbols-alist)

      ;; 🢀 🢁 🢂 🢃 🢄 🢅 🢆 🢇
      (push '("t/Int" . ?🢀) prettify-symbols-alist)
      (push '("t/Str" . ?🢁) prettify-symbols-alist)
      (push '("t/Vec" . ?🢂) prettify-symbols-alist)
      (push '("t/Map" . ?⚑) prettify-symbols-alist)

      (push '("t/U"   . ?🢅) prettify-symbols-alist) ; union
      (push '("t/IFn" . ?Ƒ) prettify-symbols-alist)
      (push '("t/Any" . ?☂) prettify-symbols-alist)
      (push '("t/ann" . ?🢄) prettify-symbols-alist)

      (push '("t/loop" . ?🔃) prettify-symbols-alist)
      (push '("nil" . ?🍀) prettify-symbols-alist)

      (push '("ta/Chan" . ?🚌) prettify-symbols-alist)
      (push '("ta/chan" . ?⛴) prettify-symbols-alist)

      ;; (push '("\"Alice\"" . ?🌓) prettify-symbols-alist)
      ;; (push '("\"Bob\"" . ?🌗) prettify-symbols-alist)

      ;; (push '("move1" . ?◒) prettify-symbols-alist)
      ;; (push '("move2" . ?◓) prettify-symbols-alist)
      ;; (push '("name1" . ?🙼) prettify-symbols-alist)
      ;; (push '("name2" . ?🙽) prettify-symbols-alist)

      (push '("t/defalias " . ?​) prettify-symbols-alist); 'ZERO WIDTH SPACE' (U+200B)
      (push '("t/ann " . ?​) prettify-symbols-alist); 'ZERO WIDTH SPACE' (U+200B)
      ;; (push '("defn" . ?⚑) prettify-symbols-alist); 'ZERO WIDTH SPACE' (U+200B)
      (push '("defn " . ?​) prettify-symbols-alist); 'ZERO WIDTH SPACE' (U+200B)
      (push '("def " . ?​) prettify-symbols-alist); 'ZERO WIDTH SPACE' (U+200B)
      ;; ⊢⊣⊤⊥
      ;; ⏩⏪⏫⏬
      ;; ❏❐❑❒
      ;; ⬖⬗⬘⬙
      ;; ☗⛊⛔🟆⚑
      ;; ◰ ◱ ◲ ◳
      ;; ▲▶▼◀
      ;; ◐ ◑ ◒ ◓
      ;; 🢀 🢁 🢂 🢃 🢄 🢅 🢆 🢇 🠈🠉🠊🠋
      ;; 1F650🙐🙑🙒🙓🙔🙕🙖🙗🙨🙩🙪🙫🙬🙭🙮🙯1F670🙰🙱🙲🙳🙴🙵🙶🙷🙸🙹🙺🙻🙼🙽

      ;; (push '("MOVES" . ?◀) prettify-symbols-alist)
      ;; (push '("BEATS" . ?◼) prettify-symbols-alist)
      ;; (push '("judge" . ?◼) prettify-symbols-alist)
      ;; (push '("rand-player" . ?⯁) prettify-symbols-alist)
      ;; (push '("winner" . ?⬢) prettify-symbols-alist)
      ;; (push '("judge" . ?⬬) prettify-symbols-alist)
      ;; (push '("init" . ?◢) prettify-symbols-alist)
      ;; (push '("report" . ?◖) prettify-symbols-alist)
      ;; (push '("play" . ?◔) prettify-symbols-alist)
      ;; (push '("play-many" . ?▼) prettify-symbols-alist)
      ;; (push '("t/defalias " . ?#x200B) prettify-symbols-alist); 'ZERO WIDTH SPACE' (U+200B)

      (setq is-pretty (* -1 is-pretty))
      (prettify-symbols-mode is-pretty)
      ;; (prettify-symbols-mode -1) ; disable
      ;; (prettify-symbols-mode +1) ; enable
      (message (concat "typed-unicode-symbols" (timestamp))))

    ;; (add-hook 'clojure-mode-hook typed-unicode-symbols)
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
  (use-package rainbow-delimiters :defer t :ensure t)

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
