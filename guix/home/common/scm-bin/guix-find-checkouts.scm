#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ guix-find-checkouts) -s
!#

(define-module (scm-bin guix-find-checkouts)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (dotf utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 optargs)     ; define*-public
  )

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ guix-find-checkouts) -s
!#

cd $dotf
./guix/home/common/scm-bin/guix-find-checkouts.scm

|#

(define m (module-name-for-logging))
(evaluating-module)

(define*-public (guix-find-checkouts #:rest args)
  "Find Guix and NonGuix checkout directories
Usage:

(guix-find-checkouts ...)
"
  (define repos
    '((guix-repo . "https://codeberg.org/guix/guix-mirror.git")
      (nonguix-repo . "https://gitlab.com/nonguix/nonguix"))
    )

  (let* ((guix-repo
          (assoc-ref repos 'guix-repo)
          ;; "https://codeberg.org/guix/guix-mirror.git"
          )
         (nonguix-repo
          (assoc-ref repos 'nonguix-repo)
          ;; "https://gitlab.com/nonguix/nonguix"
          )
         (checkouts-dir (string-append (getenv "HOME") "/.cache/guix/checkouts"))
         (directories (filter (lambda (d)
                               (let ((full-path (string-append checkouts-dir "/" d)))
                                 (and (file-exists? full-path)
                                      (eq? 'directory (stat:type (stat full-path)))
                                      (not (eq? 'symlink (stat:type (lstat full-path)))))))
                             (or (scandir checkouts-dir) '()))))

    (define (get-git-remotes dir)
      "Get git remote URLs for a directory"
      (let* ((git-dir (string-append dir "/.git"))
             (cmd (string-append "git --git-dir=" git-dir " remote -v"))
             (port (open-input-pipe cmd))
             (output (get-string-all port)))
        (close-pipe port)
        (if (eof-object? output) "" output)))

    (define (matches-repo? remotes-output repo-url)
      "Check if remotes output contains the repository URL"
      (string-match repo-url remotes-output))

    (define (find-checkouts dirs)
      "Find matching checkout directories"
      (fold (lambda (d acc)
              (let* ((full-path (string-append checkouts-dir "/" d))
                     (remotes (get-git-remotes full-path))
                     (co-gx-dir (car acc))
                     (co-non-gx-dir (cadr acc)))
                (cond
                  ((and (not co-gx-dir) (matches-repo? remotes guix-repo))
                   (format #t "set coGxDir    \"~a\"~%" full-path)
                   (list full-path co-non-gx-dir))
                  ((and (not co-non-gx-dir) (matches-repo? remotes nonguix-repo))
                   (format #t "set coNonGxDir \"~a\"~%" full-path)
                   (list co-gx-dir full-path))
                  (else acc))))
            '(#f #f)
            dirs))

    (let ((result (find-checkouts directories)))
      (let ((co-gx-dir (car result))
            (co-non-gx-dir (cadr result)))
        (when (and co-gx-dir co-non-gx-dir)
          (format #t "Guix    checkout directory: ~a~%" co-gx-dir)
          (format #t "NonGuix checkout directory: ~a~%" co-non-gx-dir))
        result))))

(define*-public (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((comp
    (partial apply guix-find-checkouts)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
