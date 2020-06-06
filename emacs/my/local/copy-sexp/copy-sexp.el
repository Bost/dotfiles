;;; copy-sexp.el --- Briefly highlights copied sexp.

(require 'eval-sexp-fu)
(require 'smartparens)

(defun cs/bounds-nearest-sexp (&optional back)
  "Returns (BEGIN . END) of the nearest balanced sexp after (before) the point."
  (let ((sexp (sp-get-thing back)))
    (cons (sp-get sexp :beg) (sp-get sexp :end))))

(defun cs/initialize-smartparens ()
  (define-eval-sexp-fu-flash-command sp-copy-sexp
    (eval-sexp-fu-flash (cs/bounds-nearest-sexp)))
  (define-eval-sexp-fu-flash-command sp-backward-copy-sexp
    (eval-sexp-fu-flash (cs/bounds-nearest-sexp t))))

;; (eval-after-load 'eval-sexp-fu
;;   '(cs/initialize-smartparens))

;; (eval-after-load 'smartparens
;;   '(cs/initialize-smartparens))

(provide 'copy-sexp)

;;; copy-sexp.el ends here
