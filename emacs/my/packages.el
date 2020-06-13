;;; packages.el --- my layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author:  <bost@bost-new-64>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my/pre-init-PACKAGE' and/or
;;   `my/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-packages
  '(
    simple
    (copy-sexp :location local)
    )
  "The list of Lisp packages required by the my layer.

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
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format"
  )

(setq
 my/line-numbers '(spacemacs/toggle-relative-line-numbers-on
                   spacemacs/toggle-relative-line-numbers-off
                   spacemacs/toggle-line-numbers-on
                   spacemacs/toggle-line-numbers-off)
 my/curr-line-number-mode nil

 my/defun-narrow-modes '(narrow-to-defun
                         widen)

 my/curr-defun-narrow-mode nil
 my/narrowed-to-defun nil
 my/iedit-mode nil)

(defun my/post-init-simple ()
  )

(defun my/init-copy-sexp ()
  ;; :config (cs/initialize-smartparens) is not needed, since there's
  ;; `eval-after-load' in the package
  (use-package copy-sexp))

;;; packages.el ends here
