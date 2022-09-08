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
    simple
    (copy-sexp
     :location
     ;; "~/dev/copy-sexp/"
     (recipe :fetcher github :repo "Bost/copy-sexp"))
    drag-stuff
    engine-mode
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

(defun my=tweaks/post-init-simple ()
  )

(defun my=tweaks/init-jump-last ()
  (use-package jump-last))

(defun my=tweaks/init-kill-buffers ()
  (use-package kill-buffers))

(defun my=tweaks/post-init-drag-stuff ()
  (use-package drag-stuff)

  (defun my=drag-stuff-up (arg)
    "Drag stuff ARG lines up."
    (interactive "p")
    (drag-stuff--execute (drag-stuff-line-up (- arg))))

  (defun my=drag-stuff-down (arg)
    "Drag stuff ARG lines down."
    (interactive "p")
    (drag-stuff--execute (drag-stuff-line-down arg)))
  )

(defun my=tweaks/init-copy-sexp ()
  ;; :config (cs/initialize-smartparens) is not needed, since there's
  ;; `eval-after-load' in the package
  (use-package copy-sexp))

;;; engine-mode extension:
(defun my=tweaks/post-init-engine-mode ()
  ;; (defvar my=engine/search-engine 'engine/search-duck-duck-go)
  ;; (setq my=engine/search-engine 'engine/search-wikipedia)
  (defvar my=engine/search-engine 'engine/search-google))

(defun my=engine/browse-url (browser-fun)
  "Function evaluation doesn't continue to the end if `browse-url'
is aborted by ~C-g~."
  (message "equal: %s, bubf %s, bf %s; my=bubf %s"
           (equal browse-url-browser-function browser-fun)
           browse-url-browser-function
           browser-fun
           my=browse-url-browser-function)
  (if (equal browse-url-browser-function browser-fun)
      (browse-url (car (browse-url-interactive-arg
                        (format "[%s] Browse URL: "
                                browse-url-browser-function))))
    ;; TODO try to use (defadvice before and after ...)
    (progn
      (setq browse-url-browser-function browser-fun)
      (browse-url (car (browse-url-interactive-arg
                        (format "[%s] Browse URL: "
                                browse-url-browser-function))))
      (setq browse-url-browser-function my=browse-url-browser-function))))

(defun my=engine/search-region ()
  "Select text as if done from the insert state."
  (funcall my=engine/search-engine
           (read-string
            "Search region: "
            (buffer-substring-no-properties (region-beginning)
                                            (region-end)))))

(defun my=engine/search-default ()
  (funcall my=engine/search-engine
           (read-string "Search thing-at-point: "
                        (thing-at-point 'symbol))))


(defun my=engine/search-or-browse (&optional arg)
  "'&optional arg' must be declared otherwise the key binding doesn't work.
Selected text has higher prio than url. YouTube urls are opened
with the `browse-url-firefox-program', otherwise use
`browse-url-browser-function'."
  (interactive "p")
  (cond
   ((or (region-active-p) (evil-visual-state-p))
    (my=engine/search-region))

   ((let ((prefix (thing-at-point 'url)))
      (or
       (string-prefix-p "https://youtu.be" prefix)
       (string-prefix-p "https://www.youtube" prefix)))
    (my=engine/browse-url 'browse-url-firefox))

   ((string-prefix-p "http" (thing-at-point 'url))
    (my=engine/browse-url my=browse-url-browser-function))

   (t
    (my=engine/search-default))))
