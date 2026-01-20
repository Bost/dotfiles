;;; Reuse Guix's CLI as the API (the Scheme modules are not really a stable
;;; public interface and keep moving around)

(define-module (dotf attest new-commits)
  #:use-module (dotf attest curr-commit)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 pretty-print)
  #:export
  (
   new-commits
   ))

(define (lines->list port)
  (let loop ((acc '()))
    (let ((l (read-line port)))
      (if (eof-object? l) (reverse acc) (loop (cons l acc))))))

(define (git-lines repo . args)
  ;; -C Run as if git was started in <path> instead of the current working dir.
  (let* ((p (apply open-pipe* OPEN_READ "git" "-C" repo args))
         (ls (lines->list p)))
    (close-pipe p)
    ls))

(define (git-one repo . args)
  ;; -C Run as if git was started in <path> instead of the current working dir.
  (let* ((p (apply open-pipe* OPEN_READ "git" "-C" repo args))
         (s (read-line p)))
    (close-pipe p)
    s))

(define (commits-between repo from to)
  ;; to can be "HEAD" or "origin/master" etc.
  (git-lines repo "rev-list" "--reverse" (string-append from ".." to)))

(define (commit-metadata repo sha)
  ;; ISO-ish date makes your parsing easier than custom formats
  (let* ((fmt "%H\t%aI\t%cI\t%an\t%ae\t%cn\t%ce")
         ;; -s, --no-patch
         ;;   output, or to cancel the effect of options like --patch, --stat
         ;;   earlier on the command line in an alias.
         (line (git-one repo "show" "-s" (string-append "--format=" fmt) sha))
         (parts (string-split line #\tab)))
    parts))

;; Example:
;; See the fish-shell functions guix-find-checkouts
(define (repo) (string-append
              "/home/bost/.cache/guix/checkouts/"
              "lmgz3ewobtxzz4rsyp72z7woeuxeghzyukvmmnxlwpobu76yyi5a"))
(define (new-shas) (commits-between (repo) (curr-commit) "HEAD"))
(define (new-commits)
  (map (lambda (sha)
         (let ((m (commit-metadata (repo) sha)))
           ;; build your plist shape here
           m))
       (new-shas)))
