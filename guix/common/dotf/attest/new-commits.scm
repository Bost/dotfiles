;;; Reuse Guix's CLI as the API.
;;;
;;; Goal:
;;; - avoid reimplementing Guix's channel update logic
;;; - reuse the checkouts that `guix pull` stores under
;;;   $XDG_CACHE_HOME/guix/checkouts/
;;;
;;; Approach:
;;; - run `guix pull --dry-run` to let Guix resolve the target commits
;;; - locate the corresponding checkout directory for the target commit
;;; - use `git` locally to enumerate commits and extract metadata

(define-module (dotf attest new-commits)
  #:use-module (dotf attest curr-commit)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-88)
  #:use-module (ice-9 pretty-print)
  #:use-module (guix channels) ; %default-guix-channel
  #:export
  (
   ;; New commits for the current Guix channel as determined by
   ;; `guix pull --dry-run`.
   new-commits
   ))

;; Keep keys descriptive here; your main module can translate them to your
;; compact :s/:tc/... representation if you want.
(read-set! keywords 'prefix)
(define ksha             :s)
(define kauthored        :ta)
(define kcommitted       :tc)
(define kauthor-name     :an)
(define kauthor-email    :ae)
(define kcommitter-name  :cn)
(define kcommitter-email :ce)

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

(define (xdg-cache-home)
  (or (getenv "XDG_CACHE_HOME")
      (string-append (or (getenv "HOME") ".") "/.cache")))

(define (guix-checkouts-dir)
  (string-append (xdg-cache-home) "/guix/checkouts"))

(define (directory-exists? path)
  (and (file-exists? path) (eq? 'directory (stat:type (stat path)))))

(define (list-directories path)
  (if (directory-exists? path)
      (filter (lambda (name)
                (let ((p (string-append path "/" name)))
                  (and (not (member name '("." "..")))
                       (directory-exists? p))))
              (scandir path))
      '()))

(define (run-lines prog . args)
  (let* ((p (apply open-pipe* OPEN_READ prog args))
         (ls (lines->list p)))
    (close-pipe p)
    ls))

(define (extract-first-hex40 s)
  (let* ((rx (make-regexp "([0-9a-f]{40})"))
         (m  (regexp-exec rx s)))
    (and m (match:substring m 1))))

(define* (guix-pull-target-commit
          #:key
          (channel-url (guix-channel-url))
          (channel-name "guix"))
  "Return the target commit that `guix pull` would update CHANNEL-URL to.

This intentionally shells out to `guix pull --dry-run` so Guix itself resolves
channels, branches, introductions, etc.\n"
  (let* ((lines (run-lines "guix" "pull" "--dry-run" "--verbosity=2"))
         (cands (filter-map
                 (lambda (l)
                   (and (or (string-contains l channel-url)
                            ;; Fallback: some Guix versions log the channel
                            ;; name but not the URL.
                            (string-contains l channel-name))
                        (extract-first-hex40 l)))
                 lines)))
    ;; Heuristic: the last mention tends to be the resolved commit.
    (and (pair? cands) (last cands))))

(define* (find-checkout-dir commit
                            #:key (channel-url (guix-channel-url)))
  "Find a Guix checkout directory whose HEAD is COMMIT.

We additionally try to match the remote URL against CHANNEL-URL when possible.
Returns an absolute path, or #f.\n"
  (let* ((root (guix-checkouts-dir))
         (dirs (map (lambda (n) (string-append root "/" n))
                    (list-directories root))))
    (find
     (lambda (dir)
       (and (file-exists? (string-append dir "/.git"))
            (let ((head (git-one dir "rev-parse" "HEAD")))
              (and head (string-prefix? commit head)
                   ;; Best-effort URL match: ignore failures.
                   (let ((url (git-one dir "remote" "get-url" "origin")))
                     (or (not url)
                         (string-contains url channel-url))))))
     dirs))))

(define (commits-between repo from to)
  ;; to can be "HEAD" or "origin/master" etc.
  (git-lines repo "rev-list" "--reverse" (string-append from ".." to)))

(define (commit-metadata repo sha)
  ;; ISO-8601 timestamps (%aI/%cI) are easy to parse.
  (let* ((fmt "%H\t%aI\t%cI\t%an\t%ae\t%cn\t%ce")
         ;; -s, --no-patch
         ;;   output, or to cancel the effect of options like --patch, --stat
         ;;   earlier on the command line in an alias.
         (line (git-one repo "show" "-s" (string-append "--format=" fmt) sha))
         (parts (string-split line #\tab)))
    (match parts
      ((sha authored committed an ae cn ce)
       (list ksha sha
             kauthored authored
             kcommitted committed
             kauthor-name an
             kauthor-email ae
             kcommitter-name cn
             kcommitter-email ce))
      (_
       (error "new-commits: unexpected git show output" parts)))))

(define* (repo #:key (channel-url (guix-channel-url)))
  (let* ((target (guix-pull-target-commit #:channel-url channel-url)))
    (or (and target (find-checkout-dir target #:channel-url channel-url))
        (error "new-commits: could not find a matching Guix checkout"
               (list #:channel-url channel-url #:target target)))))

(define* (new-shas #:key
                   (channel-url (guix-channel-url))
                   ;; If you already computed (repo ...), pass it to avoid
                   ;; running `guix pull --dry-run` twice.
                   (repo #f))
  (let* ((r    (or repo (repo #:channel-url channel-url)))
         (from (curr-commit))
         (to   (or (guix-pull-target-commit #:channel-url channel-url)
                   "HEAD")))
    (commits-between r from to)))

(define* (new-commits #:key (channel-url (guix-channel-url)))
  "Return a list of metadata plists for commits newer than the current Guix
channel commit, for the commit `guix pull --dry-run` would advance to.

Each element is a plist with keys:
  :sha :authored :committed :author-name :author-email :committer-name :committer-email"
  (let* ((r    (repo #:channel-url channel-url))
         (shas (new-shas #:channel-url channel-url #:repo r)))
    (map (lambda (sha) (commit-metadata r sha)) shas)))
