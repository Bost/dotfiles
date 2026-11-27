(define-module (scm-bin git-apply)
;;; All used modules must be present in (@(services cli-utils) common-modules)
  #:use-module (bost common utils)
  )

#|

#!/usr/bin/env -S guix repl --
!#

cd $dotf
# not '(apply main (command-line))'
echo -e "\n(main (command-line))" >> ./guix/home/common/scm-bin/git-apply.scm
./guix/home/common/scm-bin/git-apply.scm outbox/1-of-2-foo.patch outbox/2-of-2-bar.patch

|#

(define m (module-name-for-logging))
(evaluating-module)

(define usage
  "Usage: git-apply PATCH...
Apply each PATCH, in the given order, with `git am' in the current
repository and rename it to PATCH.applied. Stop at the first failure; the
failed patch keeps its name. Not to be confused with `git apply', which
changes only the working tree.
")

(define-public (main args)
  "Apply the patches in (cdr ARGS), see `usage'. Exit with git's exit code on
the first failure.
(main (list \"<ignored>\" \"outbox/1-of-1-foo.patch\"))"
  (let [(patches (cdr args))]
    (when (or (null? patches)
              (member (car patches) '("-h" "--help")))
      (format #t usage)
      (exit (if (null? patches) 1 0)))
    (for-each
     (lambda (patch)
       (unless (file-exists? patch)
         (format #t "git-apply: no such patch: ~a\n" patch)
         (exit 1))
       (let [(code (wait-status->exit-code
                    (exec-system* #:verbose #t #:split-whitespace #f
                                  "git" "am" patch)))]
         (unless (zero? code)
           ;; git am prints how to go on: --continue, --skip, --abort
           (format #t "git-apply: stopped at ~a\n" patch)
           (exit code))
         (format #t "git-apply: applied, renamed to ~a.applied\n" patch)
         (rename-file patch (str patch ".applied"))))
     patches)))
(testsymb 'main)

(module-evaluated)
