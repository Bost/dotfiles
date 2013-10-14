;; (setq url-proxy-services
;;       '(("https" . "192.168.2.105:3128")
;;         ("http" . "192.168.2.105:3128")))

(setq inhibit-splash-screen t)
;; (load-theme 'light-blue)  ;(disable-theme 'light-blue)  (enable-theme 'light-blue)
(load-theme 'deeper-blue)    ;(disable-theme 'deeper-blue) (enable-theme 'deeper-blue)
;; (load-theme 'misterioso)  ;(disable-theme 'misterioso)  (enable-theme 'misterioso)
;; (load-theme 'whiteboard)  ;(disable-theme 'whiteboard)  (enable-theme 'whiteboard)
;; test rainbow parenthesis:
;; (((((((())))))))   [[[[[[[[]]]]]]]]   {{{{{{{{}}}}}}}}

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; activate all the packages (in particular autoloads)
(package-initialize)
;; (package-refresh-contents)

(display-battery-mode 1)
(size-indication-mode 1)  ; filesize indicator

(eval-after-load "paredit.el"
  '(require 'paredit-menu))


(global-set-key (kbd "s-<left>") 'paredit-backward-slurp-sexp)
(global-set-key (kbd "s-<right>") 'paredit-backward-barf-sexp)
;; org-babel-clojure
(require 'ob-clojure)
(global-set-key (kbd "s-t") 'clojure-jump-between-tests-and-code)
;; Attention defaults are:
;;     C-c C-l: (nrepl-load-file FILENAME)
;;     C-c C-k: (nrepl-load-current-buffer)

(defun nrepl-save-and-load-current-buffer ()
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (nrepl-load-file (buffer-file-name))
  ;; (nrepl-switch-to-relevant-repl-buffer nil)
  )

(defun emacs-lisp-mode-keys ()
  "Modify keymaps used by `emacs-lisp-mode'."
  (local-set-key (kbd "s-e") 'eval-last-sexp)
  )
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-keys)

(defun skewer-mode-keys ()
  "Modify keymaps used by `skewer-mode'."
  (local-set-key (kbd "s-e") 'skewer-eval-last-expression)
  (local-set-key (kbd "s-x") 'skewer-eval-defun)
  (local-set-key (kbd "s-l") 'skewer-load-buffer)
  )
;; skewer works on top of js2-mode
;; (add-hook 'js2-mode-hook 'skewer-mode-keys)
(add-hook 'skewer-mode-hook 'skewer-mode-keys)


(defun nrepl-eval-last-expression-in-repl ()
  "This doesn't work"
  (interactive)
  (evil-visual-char)
  (evil-jump-item)
  ;; (clipboard-kill-ring-save)
  ;; (clipboard-kill-region)
  ;; (nrepl-switch-to-relevant-repl-buffer)
  ;; (clipboard-yank)

  ;; (global-set-key [(shift delete)] 'clipboard-kill-region)
  ;; (global-set-key [(control insert)] 'clipboard-kill-ring-save)
  ;; (global-set-key [(shift insert)] 'clipboard-yank)
  )

(defun clojure-mode-keys ()
  "Modify keymaps used by `clojure-mode'."
  (local-set-key (kbd "s-r") 'nrepl-eval-last-expression-in-repl)
  (local-set-key (kbd "s-e") 'nrepl-eval-last-expression)
  (local-set-key (kbd "s-z") 'nrepl-switch-to-relevant-repl-buffer)
  (local-set-key (kbd "s-l") 'nrepl-save-and-load-current-buffer)
  (local-set-key (kbd "s-n") 'nrepl-set-ns)
  (local-set-key (kbd "s-.") 'nrepl-jump)
  (local-set-key (kbd "s-,") 'nrepl-jump-back)
  )
(add-hook 'clojure-mode-hook 'clojure-mode-keys)

(defun nrepl-mode-keys ()
  "Modify keymaps used by `nrepl-mode'."
  (local-set-key (kbd "s-z") 'nrepl-switch-to-last-clojure-buffer)
  (local-set-key (kbd "s-.") 'nrepl-jump)
  (local-set-key (kbd "s-,") 'nrepl-jump-back)
  )
(add-hook 'nrepl-mode-hook 'nrepl-mode-keys)

;; (global-set-key (kbd "s-.") 'nrepl-jump)
;; (global-set-key (kbd "s-,") 'nrepl-jump-back)

(defun nrepl-interaction-mode-keys ()
  "Modify keymaps used by `nrepl-interaction-mode'."
  ;; (local-set-key (kbd "s-o") 'mnrepl-jump)
  )
(add-hook 'nrepl-interaction-mode-hook 'nrepl-interaction-mode-keys)


;; hide the *nrepl-connection* and *nrepl-server* buffers from
;; appearing in some buffer switching commands like
;; switch-to-buffer(C-x b) like this:
;; (setq nrepl-hide-special-buffers t)

(require 'auto-complete-config)
(ac-config-default)

;; M-x linum-relative-toggle
(require 'linum-relative)
;; show line number on the left side
(global-linum-mode t)

;; TODO what is incomplete-mode good for?
;; (icomplete-mode 99)  ; turn on icomplete-mode

;; press y/n instead of typing yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; reload all buffers when the file is changed
(global-auto-revert-mode t)

;; this is for the emacs code browser
(setq stack-trace-on-error t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(csv-separators (quote (";")))
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote ("~/dev/webcli")))
 '(global-hl-line-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil nil (tool-bar)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark goldenrod"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "goldenrod"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "light goldenrod"))))
 '(region ((t (:background "#006400")))))

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

;;(desktop-load-default)
;;(desktop-read)

(require 'org-install)
(org-babel-do-load-languages
'org-babel-load-languages
'(
  (sh . t)
  (python .t)
  (R . t)
  (ruby . t)
  (ditaa . t)
  (dot . t)
  (sqlite . t)
  (perl . t)
  ))

;; Add shortcuts for ogr-agenda
;(global-set-key "\C-cl" 'org-store-link)
;(global-set-key "\C-cc" 'org-capture)
;(global-set-key "\C-ca" 'org-agenda)

; Setup custom shortcuts
;(global-set-key "\C-x\C-g" 'goto-line)
;(global-set-key [f1] 'compile)
;(global-set-key [f2] 'next-error)

;(add-to-list 'load-path "~/.emacs.d/edit-server/")

(autoload 'magit-status "magit" nil t)
;; (global-set-key [f6] 'split-window-horizontally)
(global-set-key [f6] 'magit-status)

;; font size
(set-face-attribute 'default nil :height 120)

;; highlight current line - this is probably not needed in the default face
;; (global-hl-line-mode 1)
(column-number-mode 1)

(setq sql-db2-program "/cygdrive/c/winapp/IBM/SQLLIB_9.5/BIN/db2cmd.exe")

;; -t: semicolon is the command line terminator.
;; default is end-of-line as a SQL statement terminator
(setq sql-db2-options '("-c" "-i" "-w" "db2" "-tv"))

;; no line wrap
(setq default-truncate-lines t)
(define-key global-map [f5] 'toggle-truncate-lines)

(defun cygpath ()
  (setq path (thing-at-point 'line))
  (split-window-below)
  (eshell)
  (interactive)
  (insert (concat "cygpath -u " path))
  (eshell-send-input))

(global-set-key [f7] 'cygpath)

(global-set-key [f8] 'transpose-frame)

(defun exec-sql ()
  ;;(windmove-up)
  (save-buffer)
  (windmove-down)
  (goto-char (point-max))
  (interactive)
  ;(insert "db2 -vf c:\\cygwin\\home\\svo02896\\dev\\txrating\\tmp.sql -t")
  (insert "db2 -vf c:\\cygwin\\home\\svo02896\\dev\\credit\\script.sql -t")
  (eshell-send-input)
  (windmove-up))

(defun comment-sql-line ()
  (setq current-point (point))
  (move-beginning-of-line nil)
  (insert "--")
  (goto-char current-point))

(defun clean-whitespaces ()
  (setq current-point (point))
  (goto-char (point-min))
  (replace-regexp "\t" "    ")
  (goto-char (point-min))
  (replace-regexp "[\t ]+$" "")
  (save-buffer)
  (goto-char current-point))


;; buffer switching
;; (iswitchb-mode t)			; I think this is the default
(ido-mode t)				; probably somehow better

;; (desktop-save-mode 1)

;; (defun save-macro (name)
;;     "save a macro. Take a name as argument
;;      and save the last defined macro under
;;      this name at the end of your .emacs"
;;      (interactive "SName of the macro :")  ; ask for the name of the macro
;;      (kmacro-name-last-macro name)         ; use this name for the macro
;;      (find-file "~/.emacs")                ; open ~/.emacs or other user init file
;;      (goto-char (point-max))               ; go to the end of the .emacs
;;      (newline)                             ; insert a newline
;;      (insert-kbd-macro name)               ; copy the macro
;;      (newline)                             ; insert a newline
;;      (switch-to-buffer nil))               ; return to the initial buffer

;; setting the PC keyboard's various keys to
;; Super or Hyper, for emacs running on Windows.
;; run Local Group Policy Editor (gpedit.msc) -> User Configuration
;; -> Administrative Templates -> Windows Components -> Windows Explorer
;; -> Turn off Windows+X hotkeys, set it to 'Not configured' and log off
(setq w32-pass-lwindow-to-system nil
      w32-pass-rwindow-to-system nil
      w32-pass-apps-to-system nil
      w32-lwindow-modifier 'super ; Left Windows key
      w32-rwindow-modifier 'super ; Right Windows key
      w32-apps-modifier 'hyper) ; Menu key

;(global-set-key (kbd "M-s") 'save-buffer)
;; s-s is here just to have consistent key mapping.
;; If it's gonna work I can use M-s for something else
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-f") 'find-file)
(global-set-key (kbd "s-c") 'kill-ring-save) ; copy
(global-set-key (kbd "s-x") 'kill-region)    ; cut
(global-set-key (kbd "s-v") 'yank)           ; paste
(global-set-key (kbd "s-b") 'ido-switch-buffer)
;(global-set-key (kbd "s-k") 'ido-kill-buffer)
(global-set-key (kbd "s-k") 'kill-this-buffer)

(global-set-key (kbd "s-q") 'other-window)   ; (kbd "s-<tab>") does not work
(global-set-key (kbd "<S-iso-lefttab>") 'other-window)

(global-set-key (kbd "s-0") 'delete-window)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-3") 'split-window-right)

(global-set-key (kbd "s-h") 'describe-key)

;; (define-key global-map [f5] 'toggle-truncate-lines)

(require 'helm-files)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
;; (setq helm-c-locate-command "locate-with-mdfind %.0s %s")
;; (loop for ext in '("\\.swf$" "\\.elc$" "\\.pyc$")
;;       do (add-to-list 'helm-c-boring-file-regexp-list ext))
(global-set-key (kbd "M-p") 'helm-for-files)
; TODO helm-mode: <tab> should work in minibuffer as without helm
;; (helm-mode 1)

(require 'move-text)
(move-text-default-bindings)

(defun kill-line-backward (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

;; C-S-backspace is the default key binding for kill-whole-line
(global-set-key (kbd "<C-S-backspace>") 'kill-line-backward)
(global-set-key (kbd "<C-S-delete>") 'kill-line)

(put 'upcase-region 'disabled nil)

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

;; When moving to parent directory by `^´, Dired by default creates a
;; new buffer for each movement up. The following rebinds `^´ to use
;; the same buffer
;; (add-hook 'dired-mode-hook
;;  (lambda ()
;;   (define-key dired-mode-map (kbd "^")
;;     (lambda () (interactive) (find-alternate-file "..")))
;;   ; was dired-up-directory
;;  ))
(toggle-diredp-find-file-reuse-dir 1)
;; (diredp-find-file-reuse-dir-buffer 1)
;; (diredp-mouse-find-file-reuse-dir-buffer [mouse-1])
;; (diredp-subst-find-alternate-for-find)

;; layout management
(winner-mode 1)

(require 'evil-numbers)
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

(global-set-key (kbd "s-+") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "s--") 'evil-numbers/dec-at-pt)

(global-set-key (kbd "<C-kp-add>")      'evil-numbers/inc-at-pt)
(global-set-key (kbd "<C-kp-subtract>") 'evil-numbers/dec-at-pt)
(global-set-key (kbd "<s-kp-add>")      'evil-numbers/inc-at-pt)
(global-set-key (kbd "<s-kp-subtract>") 'evil-numbers/dec-at-pt)

;; or only in evil’s normal state:
;; (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
;; (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; enable global-evil-leader-mode before evil-mode, otherwise
;; evil-leader won’t be enabled in initial buffers (*scratch*, *Messages*, ...)
(global-evil-leader-mode)

(require 'evil)
(evil-mode 1)

(define-key evil-normal-state-map (kbd "<C-O>") 'evil-jump-forward)
(define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)
(global-set-key (kbd "<C-kp-multiply>") 'highlight-symbol-at-point)
(global-set-key (kbd "<S-delete>") 'kill-line)

(setq evil-leader/in-all-states t)
;; global-evil-leader-mode must be enabled before evil-mode
;; (global-evil-leader-mode)

;; (setq evil-leader/no-prefix-mode-rx nil)
;; (setq evil-leader/non-normal-prefix "C-")
;; (message
;;  (concat evil-leader/non-normal-prefix evil-leader/leader))

(evil-leader/set-key
  "wr" 'toggle-truncate-lines
  "dd" 'kill-whole-line
)


(add-to-list 'load-path "~/.emacs.d/elpa/transpose-frame/")
(require 'transpose-frame)

(setq display-time-24hr-format 1)
(display-time-mode 1)

(defun back-window ()
  ;; opposite of other-window
  (interactive)
  (other-window -1))

;; cycle through buffers with Ctrl-Tab / Shift-Ctrl-Tab
(global-set-key (kbd "<C-tab>") 'bury-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'unbury-buffer)

; (define-key evil-normal-state-map [tab] #'next-buffer)
;(define-key evil-normal-state-map [tab] #'other-window)
; (define-key evil-normal-state-map [backtab] #'back-window)

;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                                 ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                                 (t default-color))))
                (set-face-background 'mode-line (car color))
                (set-face-foreground 'mode-line (cdr color))))))

(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(define-key global-map (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

(setq default-directory "~/dev")

(defun auto-upgrade ()
  (interactive)
  (package-list-packages)
  (package-menu-mark-obsolete-for-deletion)
  (package-menu-mark-upgrades)
  (package-menu-execute))


(setq gui-elements -1)
(menu-bar-mode gui-elements)
(scroll-bar-mode gui-elements)


(defun gui-toggle ()
  (interactive)
  (setq gui-elements (* -1 gui-elements))
  (menu-bar-mode gui-elements)
  (toggle-scroll-bar gui-elements)
  (message "gui-elements %s"
           (if (= 1 gui-elements) "enabled" "disabled")))

(global-set-key [f9] 'auto-upgrade)
;; (global-set-key [f10] 'menu-bar-mode)
(global-set-key [f10] 'gui-toggle)

; pretty syntax highlighting everywhere
(global-font-lock-mode t)


;; (setq debug-on-error t)

;; (setq inferior-lisp-program "browser-repl")
;; (setq inferior-lisp-program "cljs-repl")
;; (message (concat "inferior-lisp-program: " inferior-lisp-program))

(defun cljs-repl ()
  (interactive)
  (setq inferior-lisp-program "cljs-repl")
  (run-lisp))

(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
     major-mode))

;; (setq inferior-lisp-buffer "browser-repl")
;; (message inferior-lisp-buffer)

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-all-like-this-in-defun)

;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-M->") 'mc/unmark-next-like-this)

(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-<") 'mc/unmark-previous-like-this)

;; (global-set-key (kbd "C->") 'mc/mark-next-word-like-this)
;; (global-set-key (kbd "C-M->") 'mc/unmark-next-word-like-this)

;; (global-set-key (kbd "C-<") 'mc/mark-previous-word-like-this)
;; (global-set-key (kbd "C-M-<") 'mc/unmark-previous-word-like-this)

(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-M-<") 'mc/unmark-all-like-this)

(global-set-key (kbd "<f2>") 'ace-jump-mode)

;; (require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;; yasnippets does not to work
;; (add-to-list 'load-path
;;           "~/.emacs.d/elpa/clojure-snippets-20130403.2046/snippets/clojure-mode")
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; (define-key yas-minor-mode-map (kbd "s-y") 'yas/expand)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program
      ;; "chromium-browser" does not work properly on ubuntu 13.10
      ;"chrome" ; cygwin
      "google-chrome")

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(defun copy-line (next)
  (interactive)
  (kill-whole-line)
  (yank)
  (yank)
  (previous-line)
  ;; (message (if (= 1 next) "next" "previous"))
  (if (= 1 next) (previous-line)))

(defun copy-line-goto-next ()
  (interactive)
  (copy-line 1))

(defun copy-line-goto-previous ()
  (interactive)
  (copy-line -1))

(global-set-key (kbd "C-s-<up>") 'copy-line-goto-next)
(global-set-key (kbd "C-s-<down>") 'copy-line-goto-previous)


(define-abbrev-table 'global-abbrev-table
  '(("alpha" "α")
    ("beta" "β")
    ("gamma" "γ")
    ("theta" "θ")
    ("inf" "∞")
    ("leq" "≤")
    ("geq" "≥")

    ("ar1" "→")
    ("1ar1" "↔")
    ("ar2" "⇒")
    ("2ar" "⇐")
    ("2ar2" "⇔")
    ("exs" "∃")
    ("frll" "∀")
    ("ie" "i.e.")
    ("eg" "e.g.")  ;; exempli gration - for example
    ("lam" "λ")

    ("hmm" "homomorphism")
    ("Hmm" "Homomorphism")
    ("ism" "isomorphism")
    ("Ism" "Isomorphism")
    ("ctg" "category")
    ("Ctg" "Category")
    ("thr" "theory")
    ("Thr" "Theory")

    ;; javascript
    ("co" "contract")
    ("Co" "Contract")
    ("ar" "array")
    ("Ar" "Array")
    ("ob" "object")
    ("Ob" "Object")
    ("fu" "function")
    ("fr" "functor")
    ("fu" "function")
    ("fn" "function () { return ;};")
    ("hfn" "hom()function () { return ; };")
    ("re" "return")
    ("rf" "return function () { return ; };")
    ("el" "element")
    ("El" "Element")
    ("nr" "number")
    ("Nr" "Number")
    ("iso" "isomorphism")
    ("Iso" "Isomorphism")
    ("mo" "morphism")
    ("Mo" "Morphism")
    ("eq" "equal")
    ("sq" "square")
    ("Sq" "Square")
    ("ca" "category")
    ("Ca" "Category")
    ("sig" "signature")
    ("Sig" "Signature")
    ("tens" "⊗") ; tensor

    ;; SQL
    ("sl" "select * from fetch first 10 rows only;")
    ("c26"   "ADC26TDA.")
    ;; t-table
    ("tk"    "V60050") ; KONTO
    ("tv"    "V60055") ; VERTAG
    ;; c-column
    ("coi"   "OBJECTIDENT")
    ("coid"  "OBJECTIDENT")
    ("ckto"  "KONTO")
    ("cknr"  "KONTONUMMER")
    ("cktnr" "KONTONUMMER")
    ))

;; (abbrev-mode 1) ; turn on abbrev mode

(global-set-key [f11] 'abbrev-mode)
(global-set-key [f12] 'undo-tree-visualize)
(global-set-key (kbd "s-<f7>") 'whitespace-cleanup)
;; (global-set-key [scroll] 'exec-test-macro)

(defun x11-maximize-frame ()
  "Maximize the current frame (to full screen)"
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))

(x11-maximize-frame)

(require 'rainbow-delimiters)
; To enable it only in certain modes, add lines like the following:
;(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
; To enable it in all programming-related emacs modes (Emacs 24+):
;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
; To activate the mode globally, add to your init file:
(global-rainbow-delimiters-mode)
; test delimiters:
;; (((( ((( () ))) )))) [[[[ [[[ [] ]]] ]]]] {{{{ {{{ {} }}} }}}}

;; must be loaded in the end; otherwise:
;;    Symbol's function definition is void: mapcar*
;; (require 'workgroups2)

;; (desktop-save-mode t)       ; save all opened files (or disable it)
;; (setq wg-prefix-key (kbd "C-c z")
;;      wg-restore-associated-buffers nil ; all buffers opened in current WG
;;      wg-use-default-session-file nil   ; turn off for "emacs --daemon"
;;      wg-default-session-file "~/.emacs_files/workgroups"
;;      wg-use-faces nil
;;      wg-morph-on nil)    ; animation off
;; (workgroups-mode 1)       ; Activate workgroups

;; (global-set-key (kbd "<pause>")     'wg-reload-session)
;; (global-set-key (kbd "C-S-<pause>") 'wg-save-session)
;; (global-set-key (kbd "s-z")         'wg-switch-to-workgroup)
;; (global-set-key (kbd "s-/")         'wg-switch-to-previous-workgroup)
