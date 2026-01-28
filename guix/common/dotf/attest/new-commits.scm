;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; Compute the list of new Guix commits (between your current pull and the
;;; newest fetched upstream) *without* going through the expensive parts of
;;; `guix pull` (derivations/substitutes/build plan).
;;;
;;; Key idea:
;;;   1) Let Guix update channels and (re)use the checkout under
;;;      ~/.cache/guix/checkouts/ via (guix channels).
;;;   2) Enumerate commits locally with git.
;;;
;;; This does network I/O to fetch channel updates, but it should stop before
;;; any "Computing Guix derivation ..." work would happen.

(define-module (dotf attest new-commits)
  #:use-module (dotf utils)
  #:use-module (dotf tests)
  #:use-module (dotf attest utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  ;; Guix APIs (available in a Guix REPL / `guix repl`).
  #:use-module (guix store) ; with-store
  ;; #:use-module (guix describe) ; current-channels
  ;; #:autoload   (guix describe) (current-channels)
  ;; #:use-module (guix scripts pull) ; channel-list
  #:use-module (guix scripts describe) ; guix-describe
  #:use-module (dotf attest curr-commit)
  #:export
  (
   ksha
   kcommitted
   resolve-channel-checkout
   new-commits
   ))

;; Keep key names consistent with your main module.
(read-set! keywords 'prefix)
(define ksha :s)
(define kcommitted :tc)

;; --- small helpers ------------------------------------------------------

(define (run-git-to-string . args)
  "Run git ARGS and return trimmed stdout. Errors bubble up."
  ;; (format #t "[run-git-to-string]\n  git ~a\n" (string-join args))
  (let* ((port (apply open-pipe* OPEN_READ "git" args))
         (out  (read-string port))
         (st   (close-pipe port)))
    (string-trim-right out)))

(define (run-git-to-lines . args)
  "Run git ARGS and return stdout as a list of non-empty lines."
  ;; (format #t "[run-git-to-lines]\n  git ~a\n" (string-join args))
  (let* ((port (apply open-pipe* OPEN_READ "git" args))
         (txt  (read-string port))
         (st   (close-pipe port)))
    (filter (lambda (s) (not (string-null? s)))
            (string-split (string-trim-right txt) #\newline))))

(define (iso8601ish->tstp str)
  "Convert RFC3339-ish `git show --format=%cI` to your
  \"YYYY-MM-DD_HH-MM-SS\" format.

Example input:  2026-01-05T16:00:00+01:00
Output:         2026-01-05_16-00-00
"
  (let* ((s (string-trim str))
         ;; Keep only YYYY-MM-DDTHH:MM:SS (drop fractions + timezone).
         (s (if (>= (string-length s) 19)
                (substring s 0 19)
                s)))
    (string-map (lambda (ch)
                  (cond ((char=? ch #\T) #\_)
                        ((char=? ch #\:) #\-)
                        (else ch)))
                s)))

;; --- channel checkout resolution ---------------------------------------

;; (getenv "GUIX_DAEMON_SOCKET")
;; (setenv "GUIX_DAEMON_SOCKET" "/var/guix/daemon-socket/socket")
;; (define daemon (open-connection "/var/guix/daemon-socket/socket"))

(define* (resolve-channel-checkout
          #:key
          ;; Which channel to follow.
          (name  'guix)
          ;; If you pass explicit channels, we use them; otherwise use the
          ;; user's configured channels (same as `guix pull`).
          (channels (current-channels)))
  "Return the local checkout directory for CHANNEL-NAME after Guix updates
the given CHANNELS. This fetches the channel (network I/O), but it does not
compute derivations or build anything.

(resolve-channel-checkout #:name 'guix #:channels (current-channels))
"
  ;; (format #t "channels : ~a\n" (length channels))
  (string-append
   "/home/bost/.cache/guix/checkouts/"
   "lmgz3ewobtxzz4rsyp72z7woeuxeghzyukvmmnxlwpobu76yyi5a")

  ;; (with-store store
  ;;   (let* ((instances (latest-channel-instances store channels))
  ;;          (inst      (find (lambda (i)
  ;;                             (eq? (name  (channel-instance-channel i))
  ;;                                  name ))
  ;;                           instances)))
  ;;     (unless inst
  ;;       (error "resolve-channel-checkout: channel not found" name ))
  ;;     (channel-instance-checkout inst)))
  )

;; Doesn't work:
;; (with-store store
;;   (latest-channel-instances store (current-channels)))

;; --- main ---------------------------------------------------------------

(define* (new-commits
          #:key
          (name  'guix)
          (channels (current-channels)))
  "Return a list of new commits as plists:

  (list (list :s <sha-symbol> :tc <YYYY-MM-DD_HH-MM-SS>) ...)

FROM is the currently installed Guix commit (a string or symbol). The target
commit is taken from the freshly updated channel checkout.

(new-commits #:name 'guix #:channels (current-channels))
"
  (let* ((repo (resolve-channel-checkout #:name name #:channels channels))
         (from (guix-channel-commit #:name name #:channels channels))
         (to   (run-git-to-string "-C" repo "rev-parse" "HEAD")))
    (when (string=? from to)
      '())
    ;; `A..B` means "commits reachable from B, excluding those reachable from A".
    ;; We list oldest->newest for easier reasoning.
    (let* ((rev-range (string-append from ".." to))
           (shas (reverse (run-git-to-lines "-C" repo "rev-list" rev-range))))
      ;; (format #t "[new-commits] ~a .. ~a\n" from to)
      ;; (format #t "[new-commits] from..to : ~a shas\n" (length shas))
      (let* ((mk      (lambda (sha)
                        (let ((ts (run-git-to-string "-C" repo "show" "-s"
                                                     "--format=%cI" sha)))
                          (list ksha
                                sha
                                ;; (string->symbol sha)
                                kcommitted (iso8601ish->tstp ts))))))
        ;; (format #t "[new-commits] mk : ~a\n" mk)
        (map mk shas)))))
