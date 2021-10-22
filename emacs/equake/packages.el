;;; packages.el --- equake layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author:  <bost@ecke>
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
;; added to `equake-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `equake/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `equake/pre-init-PACKAGE' and/or
;;   `equake/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst equake-packages
  '(
    equake
    ;; TODO doesn't get installed due to missing package description
    ;; (init-eshell :location
    ;;              (recipe :fetcher gitlab :repo "emacsomancer/init-eshell.el"))
    )
  "The list of Lisp packages required by the equake layer.

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

(defun equake/init-equake ()
  (use-package equake
    :custom
    ;; (customize-set-variable 'equake-opacity-active 100)
    (equake-opacity-active 100)
    ;; (customize-set-variable 'equake-opacity-inactive 100)
    (equake-opacity-inactive 100)

    ;; set width a bit less than full-screen (prevent 'overflow' on multi-monitor):
    ;; (equake-size-width 0.99)

    (equake-size-height 0.5)

    ;; set distinct face for Equake: white foreground with dark blue background, and different font:
    ;; :custom-face
    ;; (equake-buffer-face
    ;;  ((t (:inherit 'default :background "#073642" :foreground "#93A1A1"))))
    ;; :config
    ;; prevent accidental frame closure:
    ;; (advice-add #'save-buffers-kill-terminal :before-while #'equake-kill-emacs-advice)
    ;; binding to restore last Equake tab when viewing a non-Equake buffer
    ;; (global-set-key (kbd "<f1>") #'equake-restore-last-etab)
    )
)

(defun equake/post-init-equake ()
  (equake-mode))

(defun equake/init-init-eshell ()
  (use-package init-eshell))

(defun equake-toggle-fullscreen ()
  (interactive)
  (if (eq equake-size-height 1)
      (custom-reevaluate-setting 'equake-size-height)
    (customize-set-variable 'equake-size-height 1))
  (frame-initialize)
  (equake-invoke))
