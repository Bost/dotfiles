

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;(require 'package)
;(add-to-list 'package-archives
;             '("marmalade" . "http://marmalade-repo.org/packages/") t)
;(package-initialize)

;(add-to-list 'package-archives
;'("technomancy" . "http://repo.technomancy.us/emacs/") t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes nil)
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


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

; Add shortcuts for ogr-agenda
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)

; Setup custom shortcuts
(global-set-key "\C-x\C-g" 'goto-line)
(global-set-key [f1] 'compile)
(global-set-key [f2] 'next-error)

(setq inferior-lisp-program "/home/bost/bin/lein repl")
