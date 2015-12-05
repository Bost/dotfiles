;;; emacs-startup.el --- startup file for emacs. -*- lexical-binding: t -*-
;;; Code:

(use-package emacs :ensure t
  :bind (;; ("C-x C-g" . goto-line)
         ;; ("<f2>"    . next-error)
         ("<f10>" . menu-bar-open) ; this is the default
         ("s-F"   . helm-find-files)
         ("s-s"   . save-buffer)
         ("s-x"   . kill-region)    ; cut
         ("s-v"   . yank)           ; paste
         ;; see evil-window-map
         ;; TODO s-q should work like C-tab if only one window is visible
         ("s-q"               . other-window)
         ("s-0"               . delete-window)
         ("s-1"               . delete-other-windows)
         ("s-R"               . rename-file-and-buffer)
         ;; ("<f3>"           . find-grep) ; Use -e '...' instead of -e "..."
         ("<f7>"              . find-file-emacs)
         ("s-k"               . close-buffer)
         ("C-s-k"             . delete-file-and-close-its-buffer)
         ("s-2"               . split-other-window-below)
         ("s-3"               . split-other-window-right)
         ("<s-f10>"           . find-zark-file)
         ("<s-f11>"           . find-emacs-init-file)
         ("<s-f12>"           . switch-to-buffer-scratch)
         ("<C-S-iso-lefttab>" . unbury-buffer)
         ("M-s-h"             . shrink-window-horizontally)
         ("M-s-l"             . enlarge-window-horizontally)
         ("M-s-j"             . enlarge-window)
         ("M-s-k"             . shrink-window)
         ("s-u"               . eval-buffer-and-notify) ; might be in lisp-mode-keys see ("s-u" . helm-surfraw)
         ("<C-up>"            . xah-backward-block)
         ("<C-down>"          . xah-forward-block)
         ("<C-prior>"         . hs-hide-block)
         ("<C-next>"          . hs-show-block)
         ;; ("<C-M-prior>"       . hs-toggle-hiding)
         ("<C-M-prior>"       . hs-hide-all)
         ("<C-M-next>"        . hs-show-all)

         ;; ("<s-delete>"     . kill-sexp)
         ("<s-backspace>"     . paredit-backward-kill-word)
         ("<s-delete>"        . paredit-forward-kill-word)
         ;; default key binding; transpose current sexp with sexp on the right from cursor
         ;; ("C-M-t"           . transpose-sexp)

         ;; ("<C-kp-multiply>" . highlight-symbol-at-point)
         ;; ("<s-f10>"         . gui-toggle) ;; shows also scrollbars
         ;; ("<s-tab>"         . other-window)

         ;;  ((kbd "C-<f11>") . (lambda ()
         ;;                (interactive)
         ;;                (shell-command
         ;;                 ;; "cvs-ci-hooks.sh"
         ;;                 "")))
         ;;
         ;;  ((kbd "C-<f8>") . (lambda ()
         ;;               (interactive)
         ;;               (shell-command
         ;;                ;; "cvs-test.sh -lo :pserver:faizal@localhost:/myrepos"
         ;;                "")))
         ;;  ((kbd "C-<f12>") . (lambda ()
         ;;                         (interactive)
         ;;                         (shell-command
         ;;                          ;; (concat "cvs-test.sh -fr "
         ;;                          ;;         ":pserver:"
         ;;                          ;;         "rsvoboda@dlnxcvshooksdev01.ptx.fr.sopra"
         ;;                          ;;         ":2401/cvscorfja")
         ;;                          "")))
         ;;
         ;;  ((kbd "<s-f3>") . kmacro-start-macro-or-insert-counter)
         ;;  ((kbd "<s-f4>") . kmacro-end-or-call-macro)
         )
  :bind (:map emacs-lisp-mode-map
              ("s-e"   . eval-last-sexp)
              ("s-D"   . eval-defun) ; also C-M-x
              ("s-E"   . eval-defun)
              ("s-f"   . eval-defun))
  :config
  ;; (defalias 'qrr 'query-replace-regexp) ; M-x qrr
  (global-prettify-symbols-mode +1)

  (prefer-coding-system 'utf-8)
  (setq ;;setq gui-elements -1
        backup-inhibited t)
  :init
  (defun eval-buffer-and-notify ()
    (interactive)
    (eval-buffer)
    (message (concat "Buffer evaluated :timestamp \"" (timestamp) "\"")))

  (defun eval-and-replace ()
    "Replace the preceding sexp with its value."
    (interactive)
    (backward-kill-sexp)
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0)))))

  ;; Increase/decrease selective display - indentation
  (defun inc-selective-display (arg)
    (interactive "P")
    (if (numberp arg)
        (set-selective-display arg)
      (if (numberp selective-display)
          (set-selective-display (+ 2 selective-display))
        (set-selective-display 2)))
    (create-temp-selective-display-keymap))

  (defun dec-selective-display ()
    (interactive)
    (when (and (numberp selective-display)
               (> selective-display 2))
      (set-selective-display (- selective-display 2)))
    (create-temp-selective-display-keymap))

  (defun clear-selective-display ()
    (interactive)
    (when (numberp selective-display)
      (set-selective-display nil)))

  (defun create-temp-selective-display-keymap ()
    (set-temporary-overlay-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "+") 'inc-selective-display)
       (define-key map (kbd "-") 'dec-selective-display)
       (define-key map (kbd "0") 'clear-selective-display)
       map))
    (message "Type + to reveal more, - for less, 0 to reset."))

  ;; shorthand for interactive lambdas
  (defmacro interactive-lambda (&rest body)
    `(lambda ()
       (interactive)
       ,@body))

  (defun switch-to-buffer-scratch ()
    (interactive)
    (switch-to-buffer "*scratch*"))

  (defun find-emacs-init-file ()
    (interactive)
    (find-file (concat config-dir "/init.el")))

  (defun find-zark-file ()
    (interactive)
    (find-file (concat elisp-dir "/zark.el")))

  (defun split-other-window-and (f)
    (funcall f)
    (other-window 1))

  (defun split-other-window-below ()
    (interactive)
    (split-other-window-and 'split-window-below))

  (defun split-other-window-right ()
    (interactive)
    (split-other-window-and 'split-window-right))

  ;; (bind-key [C-s-left] (ignore-error-wrapper 'windmove-left))
  ;; (bind-key [C-s-right] (ignore-error-wrapper 'windmove-right))
  ;; (bind-key [C-s-up] (ignore-error-wrapper 'windmove-up))
  ;; (bind-key [C-s-down] (ignore-error-wrapper 'windmove-down))

  ;; (bind-key "s-b" 'ido-switch-buffer) ; s-b used for helm-mini
  ;; cycle through buffers with Ctrl-Tab / Shift-Ctrl-Tab

  (use-package grep+ :defer t :ensure t
    ;; :config (grepp-remove-comments)
    )

  ;; (grep-apply-setting
  ;;  'grep-find-command
  ;;  (concat "find ~/dev/cvs-1.12.13+real"
  ;;          " -type f"
  ;;          " ! -name '*~' -and ! -name '*.o' ! -name 'config.log'"
  ;;          " -and ! -name 'ChangeLog*' -exec grep -nH -e '' {} +"))

  ;; C-h clashes with "help"
  ;; (bind-key (kbd "C-h") 'find-grep) ; as in eclipse

  (defun rename-file-and-buffer ()
    "Rename the current buffer and file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
          (message "Buffer is not visiting a file!")
        (let ((new-name (read-file-name "New name: " filename)))
          (cond
           ((vc-backend filename) (vc-rename-file filename new-name))
           (t
            (rename-file filename new-name t)
            (set-visited-file-name new-name t t)))))))

  ;; disable most of this stuff early in the process so it doesn’t flicker.
  ;; (if (fboundp 'tool-bar-mode) (tool-bar-mode gui-elements))
  ;; TODO test fboundp - is it faster?
  ;; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode gui-elements))
  ;; (if (fboundp 'menu-bar-mode) (menu-bar-mode gui-elements))
  ;; (menu-bar-mode gui-elements)
  ;; (scroll-bar-mode gui-elements)

  ;; (defun gui-toggle ()
  ;;   (interactive)
  ;;   (setq gui-elements (* -1 gui-elements))
  ;;   (menu-bar-mode gui-elements)
  ;;   (toggle-scroll-bar gui-elements)
  ;;   (message "gui-elements %s"
  ;;            (if (= 1 gui-elements) "enabled" "disabled")))

  (defun xah-forward-block (&optional φn)
    "Move cursor forward to the beginning of next text block.
A text block is separated by blank lines. In most major modes,
this is similar to `forward-paragraph', but this command's
behavior is the same regardless of syntax table."
    (interactive "p")
    (search-forward-regexp "\n[\t\n ]*\n+" nil "NOERROR" φn))

  (defun xah-backward-block (&optional φn)
    "Move cursor backward to previous text block.
See: `xah-forward-block'"
    (interactive "p")
    (dotimes (ξn φn) (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR")
                         (progn
                           (skip-chars-backward "\n\t "))
                       (progn (goto-char (point-min))))))

  (defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
    "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

  (defvar current-time-format "%a %H:%M:%S"
    "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

  (defun timestamp ()
    (interactive)
    (format-time-string current-time-format (current-time)))

  (defun emacs-build-time-formated ()
    (interactive)
    (let ((ebt (format-time-string current-date-time-format
                                   emacs-build-time)))
      (message (concat "emacs-build-time: " ebt))))

  (define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)

  (defun save-macro (name)
    "Save the last defined macro under 'name' at the end of .emacs"
    (interactive "SName of the macro :")  ; ask for the name of the macro
    (kmacro-name-last-macro name)         ; use this name for the macro
    (find-file (concat config-dir ".emacs.el")) ; open .emacs/other user init file
    (goto-char (point-max))               ; go to the end of the .emacs
    (newline)                             ; insert a newline
    (insert-kbd-macro name)               ; copy the macro
    (newline)                             ; insert a newline
    (switch-to-buffer nil))               ; return to the initial buffer

  (defun close-buffer ()
    (interactive)
    (if server-buffer-clients
        (server-edit)
      (kill-this-buffer)))
  (defun delete-file-and-close-its-buffer ()
    (interactive)
    (let ((file-name (buffer-file-name (current-buffer))))
      (delete-file file-name)
      (message (concat "File deleted: " file-name)))
    (close-buffer))

  (dolist (mode '(tool-bar-mode
                  ;; menu-bar-mode
                  scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1)))

  ;; check on saving if the file contains a shebang; if yes make it executable
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

  (defun emacs-lisp-mode-keys ()
    "Modify keymaps used by `emacs-lisp-mode'."
    (if (featurep 'evil-leader)
        (evil-leader/set-key "e" 'eval-last-sexp)))
  (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-keys)

  ;; store / restore : C-x r j / C-x r j w
  ;; (window-configuration-to-register ?w)

  ;; (setq tramp-default-method "ssh")
  )

(provide 'emacs-startup)
