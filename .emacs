(setq inhibit-splash-screen t)
(load-theme 'deeper-blue t)

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(require 'auto-complete-config)
(ac-config-default)

(global-linum-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)
