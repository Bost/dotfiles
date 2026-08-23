(define-module (scm-bin git-clone)
;;; All used modules must be present in (@(services cli-utils) common-modules)
  #:use-module (bost common utils)
  #:use-module (ice-9 match)       ; match
  #:use-module (ice-9 regex)       ; make-regexp, regexp-exec, match:substring
  #:use-module (ice-9 optargs)     ; define*-public, def*-public
  )

#|

#!/usr/bin/env -S guix repl --
!#

cd $dotf
echo -e "\n(apply main (command-line))" >> ./guix/home/common/scm-bin/git-clone.scm
./guix/home/common/scm-bin/git-clone.scm https://codeberg.org/Bost/guix-guake
./guix/home/common/scm-bin/git-clone.scm https://codeberg.org/Bost/guix-guake foobar

|#

(define m (module-name-for-logging))
(evaluating-module)

;;; A subprocess can't change its parent shell's working directory, so this
;;; utility doesn't `cd' itself. Instead, when the clone succeeds, it prints
;;; ONLY the absolute path of the directory `git clone' created -- nothing
;;; else -- on stdout. A thin shell function captures that one line and does
;;; the actual `cd'; see the `gicl' function in .bashrc and
;;; .config/fish/functions/gicl.fish.
;;;
;;; The target directory is read out of git's own "Cloning into '...'"
;;; message rather than guessed by stripping the URL ourselves, so it stays
;;; correct for `git clone URL DIR', bare clones (`--bare'), etc.

(define cloning-into-rx
  (make-regexp "Cloning into (bare repository )?'([^']+)'"))

(define (absolute-path dir)
  (if (string-prefix? "/" dir)
      dir
      (str (getcwd) "/" dir)))

(def*-public (git-clone #:rest args)
  "Run `git clone ARGS...', re-emitting its combined output on the current
error port. Return the absolute path of the cloned repository, or #f if the
clone fails or the target directory cannot be determined.

Standard error is merged into standard output because `exec' captures only
standard output.

(git-clone \"https://codeberg.org/Bost/guix-guake\")       ;=> \"/home/bost/dev/dotfiles/guix-guake\"
(git-clone \"https://codeberg.org/Bost/guix-guake\" \"bar\") ;=> \"/home/me/bar\""
  (let* [(quoted-args (map (lambda (a) (str "'" (escape-single-quotes a) "'"))
                            args))
         ;; Force Git's output into the C locale so that the
         ;; "Cloning into ..." diagnostic can be parsed reliably.
         ;; `exec' captures stdout only; merge Git's stderr so that both its
         ;; diagnostics and the target-directory line appear in `results'.
         (cmd (str "LC_ALL=C git clone " (str-join " " quoted-args) " 2>&1"))
         ]
    (match (exec cmd #:return-plist #t #:verbose #f)
      [(#:retcode retcode #:results results)
       (for-each (lambda (line) (format (current-error-port) "~a\n" line))
                 results)
       (let [(found (some (lambda (line) (regexp-exec cloning-into-rx line))
                          results))]
         (cond
          [(and (zero? retcode) found)
           (absolute-path (match:substring found 2))]
          [else #f]))])))
(testsymb 'git-clone)

(define*-public (main #:rest args)
  "Usage:
(main \"<ignored>\" \"url\" [\"dir\"])"
  (let [(dir ((comp (partial apply git-clone) cdr) args))]
    (if dir
        (begin (display dir) (newline) (exit 0))
        (exit 1))))
(testsymb 'main)

(module-evaluated)
