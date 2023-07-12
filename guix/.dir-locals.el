;; To activate changes switch to any scribble buffer and reload scribble-mode:
;;   M-x scribble-mode
(
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Rebinding.html
 ;; see local-function-key-map
 ;; `M-x local-set-key RET key cmd RET' seems not to be permanent
 (nil
  .
  ((eval
    .
    (progn

      ;; from https://www.emacswiki.org/emacs/AddKeywords
      ;; (defun scribble-add-keywords (face-name keyword-rules)
      ;;   (let* ((keyword-list (mapcar #'(lambda (x)
      ;;                                    (symbol-name (cdr x)))
      ;;                                keyword-rules))
      ;;          (keyword-regexp (concat "(\\("
      ;;                                  (regexp-opt keyword-list)
      ;;                                  "\\)[ \n]")))
      ;;     (font-lock-add-keywords 'scheme-mode
      ;;                             `((,keyword-regexp 1 ',face-name))))
      ;;   (mapc #'(lambda (x)
      ;;             (put (cdr x)
      ;;                  'scheme-indent-function
      ;;                  (car x)))
      ;;         keyword-rules))

      ;; (scribble-add-keywords
      ;;  'font-lock-keyword-face
      ;;  '(
      ;;    (1 . https)
      ;;    ;; (1 . when)
      ;;    ;; (1 . unless)
      ;;    ;; (2 . let1)
      ;;    ;; (1 . error)
      ;;    ))

;;; TODO font-facing of numbers overrides comment-face. Test case:
;;; # foo 42 bar

;;; TODO does "font-lock-add-keywords 'scribble-mode" need some `eval-after-load`?
      (font-lock-add-keywords
       'scheme-mode
       `(
         ;; from scheme.el.gz
         (,(concat "(\\(def\\*?\\("
                   ;; Function names.
                   "\\(\\|-public\\|-method\\|-generic\\(-procedure\\)?\\)\\|"
                   ;; Macro names, as variable names.  A bit dubious, this.
                   "\\(-syntax\\|-macro\\)\\|"
                   ;; Class names.
                   "-class"
                   ;; Guile modules.
                   "\\|-module"
                   "\\)\\)\\>"
                   ;; Any whitespace and declared object.
                   ;; The "(*" is for curried definitions, e.g.,
                   ;;  (define ((sum a) b) (+ a b))
                   "[ \t]*(*"
                   "\\(\\sw+\\)?")
          . 'font-lock-keyword-face)
         ))

      ;; (setq-local home-dir (format "%s/guix" (getenv "dotf")))

      ;; (defun dotf=find-file-- ()
      ;;   (interactive)
      ;;   (find-file
      ;;    (format "%s/" home-dir)))

      ;; (dolist (state-map `(,scribble-mode-map))
      ;;   ;; See also `set-local-keymap'
      ;;   (bind-keys
      ;;    :map state-map
      ;;    ;; The binding description doesn't appear in the `M-x helm-descbinds'
      ;;    ;; if the binding is defined using lambda:
      ;;    ;;    ("<some-key>" . (lambda () (interactive) ...))
      ;;    ("<s-f4>"  . notes=find-file--)
      ;;    ))
      )))))
