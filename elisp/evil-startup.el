;;; evil-startup.el --- startup file for evil. -*- lexical-binding: t -*- 
;;; Code:

(use-package evil
  :ensure t
  :init
  (evil-mode 1)

  (bind-key "s-SPC" 'evil-search-highlight-persist-remove-all)
  (bind-key "C-s-t" 'evil-mode)
  (bind-key "s-;" 'evilnc-comment-or-uncomment-lines)
  (bind-key "s-z" 'evil-ace-jump-char-mode)

  ;; require for evil folding
  (add-hook 'prog-mode-hook 'hs-minor-mode)

  (use-package evil-visual-mark-mode ; TODO see helm-bookmarks
    :ensure t
    :defer t
    :init
    (evil-visual-mark-mode))

  ;; (interactive "r")
  ;; TODO backspace smartparens
  ;; (define-key evil-insert-state-map "<backspace>" 'evil-delete)
  ;; (define-key evil-insert-state-map "<delete>" 'evil-delete)
  ;; (message "evil-mode 1")

  ;; f/F/t/T; emulates vim-sneak, vim-seek for evil-mode by default
  ;; bound to s/S in normal mode and z/Z/x/X in visual or operator mode.
  ;; (use-package evil-snipe
  ;;   :config
  ;;   (progn
  ;;     (global-evil-snipe-mode 1)))

  ;; text exchange operator
  ;;(use-package evil-exchange) ; not available in melpa-stable
  ;;(setq evil-exchange-key (kbd "zx"))
  ;;(evil-exchange-install)

  (use-package evil-nerd-commenter
    :ensure t
    :init
    (bind-key "C-;" 'evilnc-comment-or-uncomment-lines)
    (bind-key "M-;" 'evilnc-comment-or-uncomment-lines))

  (use-package evil-visualstar
    :ensure t
    :defer t)

  ;; (use-package evil-jumper) ;; C-i / C-o

  ;; Set cursor colors depending on mode
  (when (display-graphic-p)
    (setq evil-emacs-state-cursor '("red" box)
          evil-normal-state-cursor '("green" box)
          evil-visual-state-cursor '("orange" box)
          evil-insert-state-cursor '("red" bar)
          evil-replace-state-cursor '("red" bar)
          evil-operator-state-cursor '("red" hollow)))

  (use-package evil-args
    :ensure t
    :init

    (use-package evil-surround
      :ensure t
      :init
      (defun visual-double-quote-string (&optional arg)
        "Select string inside double quote chars"
        (evil-normal-state)
        (interactive "p")
        (kmacro-exec-ring-item (quote ("vi\"" 0 "%d")) arg))

      (bind-key "s-\"" 'visual-double-quote-string)
      (bind-key "M-\"" 'visual-double-quote-string)
      (global-evil-surround-mode 1))

    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

    ;; bind evil-forward/backward-args
    (define-key evil-normal-state-map "L" 'evil-forward-arg)
    (define-key evil-normal-state-map "H" 'evil-backward-arg)
    (define-key evil-motion-state-map "L" 'evil-forward-arg)
    (define-key evil-motion-state-map "H" 'evil-backward-arg)

    ;; bind evil-jump-out-args
    (define-key evil-normal-state-map "K" 'evil-jump-out-args))

  (use-package evil-numbers
    :defer t
    :ensure t
    :init
    (bind-key "C-c +" 'evil-numbers/inc-at-pt)
    (bind-key "C-c -" 'evil-numbers/dec-at-pt)
    (bind-key "s-+" 'evil-numbers/inc-at-pt)
    (bind-key "s--" 'evil-numbers/dec-at-pt)
    (bind-key (kbd "<C-kp-add>")       'evil-numbers/inc-at-pt)
    (bind-key (kbd "<C-kp-subtract>")  'evil-numbers/dec-at-pt)
    (bind-key (kbd "<s-kp-add>")       'evil-numbers/inc-at-pt)
    (bind-key (kbd "<s-kp-subtract>")  'evil-numbers/dec-at-pt))

  (use-package evil-smartparens
    :defer t
    :ensure t
    :init
    ;; evil-smartparens everywhere
    ;; (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
    ;; evil-smartparens only in clojure
    (add-hook 'clojure-mode-hook #'evil-smartparens-mode)
    ;; (sp-pair "\{" "\}")
    )

  ;; enable global-evil-leader-mode before evil-mode, otherwise
  ;; evil-leader won’t be enabled in the initial buffers
  ;; (*scratch*, *Messages*, ...)
  (use-package evil-leader
    :ensure t
    :defer t
    :init
    (global-evil-leader-mode)
    (setq evil-leader/in-all-states t)

    (defun delete-rest-of-cheatsheet-entry (&optional arg)
      "kbd macro - starts in evil-normal-mode;
      delete up to the ' (single quote) character"
      (interactive "p")
      (kmacro-exec-ring-item (quote ([118 116 39 120 105] 0 "%d"))
                             arg))

    (evil-leader/set-key
      "dd" 'kill-whole-line
      ;; wr gives: (error "Key sequence w r starts with non-prefix key w")
      ;; "wr" 'toggle-truncate-lines
      "q" 'other-window
      "dr" 'delete-rest-of-cheatsheet-entry
      "SPC" 'evil-search-highlight-persist-remove-all)

    (if (featurep 'helm)
        (evil-leader/set-key
          ;; x gives: (error "Key sequence x x starts with non-prefix key x")
          ;; "x" 'helm-M-x
          ;; this doesn't help:
          ;;   (eval-after-load 'helm "x" 'helm-M-x)
          ;; although manual eval "after" helps
          "G" 'helm-google-suggest ; google auto-complete
          "g" 'helm-google         ; alternative to google-this region
          "f" 'helm-find-files
          "a" 'helm-buffers-list)
      (evil-leader/set-key
        ;; "x" 'execute-extended-command
        "f" 'find-file
        "a" 'switch-to-buffer)))

  ;; from evil-commands
  (define-key evil-normal-state-map (kbd "<C-O>") 'evil-jump-forward)

  (use-package evil-search-highlight-persist
    :ensure t
    :init
    (global-evil-search-highlight-persist t))

  ;; mode-line (pink - bottom left): 'current match/total matches'
  (use-package anzu
    :defer t
    :ensure t
    :diminish anzu-mode
    :init
    (global-anzu-mode 1)
    (use-package evil-anzu
      :ensure t)))

(provide 'evil-startup)
