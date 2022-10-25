;;; packages.el --- my=tweaks layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author:  Rostislav Svoboda <Rostislav.Svoboda@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my=tweaks-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my=tweaks/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my=tweaks/pre-init-PACKAGE' and/or
;;   `my=tweaks/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my=tweaks-packages
  '(
    ;; (equake :variables equake-default-shell 'term)
    (copy-sexp
     :location
     ;; "~/dev/copy-sexp/"
     (recipe :fetcher github :repo "Bost/copy-sexp"))
    drag-stuff
    (jump-last
     :location
     ;; "~/dev/jump-last/"
     (recipe :fetcher github :repo "Bost/jump-last"))
    (kill-buffers
     :location
     ;; "~/dev/kill-buffers/"
     (recipe :fetcher github :repo "Bost/kill-buffers")
     )
    )
  "The list of Lisp packages required by the my=tweaks layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


(setq my=iedit-mode nil)
;; (defvar my=iedit-mode nil) ;; TRY defvar

(defun my=tweaks/post-init-equake ()
  ;; Under xfce4-keyboard-settings -> Application shortcuts
  ;; set: emacsclient -n -e '(equake-invoke)'
  (equake-mode))

(defun my=tweaks/init-jump-last ()
  (use-package jump-last))

(defun my=tweaks/init-kill-buffers ()
  (use-package kill-buffers))

(defun my=tweaks/post-init-drag-stuff ()
  (use-package drag-stuff
    ;; :diminish drag-stuff-mode
    ;; hack to stop drag-stuff setting key mappings over ones our existing
    ;; :init (setq drag-stuff-modifier 'ctrl)
    :config
    (drag-stuff-global-mode 1)
    ;; activate key-bindings <M-up> <M-down> <M-right> <M-left>
    (drag-stuff-define-keys)))

(defun my=tweaks/init-copy-sexp ()
  ;; :config (cs/initialize-smartparens) is not needed, since there's
  ;; `eval-after-load' in the package
  (use-package copy-sexp))

;; The url is from
;;   ~/.spacemacs.d/layers/+web-services/search-engine/packages.el
(setq my=search-url "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")

(defun my=search-or-browse (&optional args)
  "'&optional args' must be declared otherwise the key binding doesn't work.
Selected text has higher priority than URL. A YouTube URL is
immediately opened by `browse-url-firefox', anything else is put
on prompt with the `my=search-url' prefix and handled by
`browse-url-chromium'."
  (interactive "p")
  (cond
   ((or (region-active-p) (evil-visual-state-p))
    ;; Select text as if done from the insert state.
    (browse-url-chromium
     (format my=search-url
             (read-string "[chromium] search region: "
                          (buffer-substring-no-properties (region-beginning)
                                                          (region-end))))))

   ((let ((url-string (thing-at-point 'url)))
      (or
       (string-prefix-p "https://youtu.be" url-string)
       (string-prefix-p "https://www.youtube" url-string)))
    (browse-url-firefox (thing-at-point 'url)))

   ;; test http://bla.com
   ((string-prefix-p "http" (thing-at-point 'url))
    (browse-url-chromium (thing-at-point 'url)))

   (t
    (browse-url-chromium
     (format my=search-url
             (read-string "[chromium] search thing: "
                          (thing-at-point 'symbol)))))))

