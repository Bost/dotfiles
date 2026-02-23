(define-module (kernel-utils)
  ;; #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)      ; list utilities
  #:use-module (ice-9 string-fun) ; string-contains etc.
  #:use-module (gnu system)       ; %default-kernel-arguments
  #:export
  (
   append-to-default-kernel-arguments
   )
  )

(define (kernel-arg-name arg)
  "Extract the parameter name from a kernel argument string."
  (let ((idx (string-index arg #\=)))
    (if idx (substring arg 0 idx) arg)))

(define (kernel-arg-values arg)
  "Extract the comma-separated values from a kernel argument, or '() if none."
  (let ((idx (string-index arg #\=)))
    (if idx
        (string-split (substring arg (1+ idx)) #\,)
        '())))

(define (merge-kernel-args base additional)
  "Merge two kernel argument strings with the same parameter name."
  (let* ((name     (kernel-arg-name base))
         (values   (append (kernel-arg-values additional)
                           (kernel-arg-values base)))
         (deduped  (delete-duplicates values string=?)))
    (if (null? deduped)
        name
        (string-append name "=" (string-join deduped ",")))))

(define (append-to-default-kernel-arguments additional-arguments)
  "Merge ADDITIONAL-ARGUMENTS into %default-kernel-arguments.
Matching parameters have their values merged; new parameters are added.
Result is sorted alphabetically.

(append-to-default-kernel-arguments
 (list \"modprobe.blacklist=nouveau\" \"iommu=pt\"))
;; => (\"iommu=pt\" \"modprobe.blacklist=nouveau,usbmouse,usbkbd\" \"quiet\")
"
  ((compose
    (lambda (lst) (sort lst string<?))
    (lambda (defaults)
      (fold (lambda (extra acc)
              (let* ((name    (kernel-arg-name extra))
                     (target  (find (lambda (d)
                                      (string=? (kernel-arg-name d) name))
                                    acc)))
                (if target
                    (cons (merge-kernel-args target extra)
                          (delete target acc))
                    (cons extra acc))))
            defaults
            additional-arguments)))
   %default-kernel-arguments))
