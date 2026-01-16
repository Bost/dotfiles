(define-module (dotf repos)
  #:use-module (dotf utils)
  #:use-module (dotf tests)
  #:use-module (srfi srfi-88)  ; provides keyword objects
  #:use-module (guix channels) ; %default-channels
  #:use-module (srfi srfi-19)  ; string->date
  #:use-module (ice-9 match)
  )

(define upstream
  (list
   :url (channel-url %default-guix-channel)
   :finger-print ""
   ;; git ls-remote --refs https://git.guix.gnu.org/guix.git
   :commits
   '(
;;;                    3a100 3a200 3a300
;;;                   /
;;;                  /
    10000 20000 30000
;;;                  \                         3b4a1 3b4a2
;;;                   \                       /
                       3b100 3b200 3b300 3b400
;;;                                           \
                                               3b4b1 3b4b2 3b4b3 3b4b4 3b4b5
    )
   ))

;; commits: `git ls-remote`
(define attesters
  (list
;;; relevant attesters, not auto-mirrors
   (list
    :url "https://github.com/Millak/guix"
    :finger-print ""
    :tstp "2026-01-12_19-07-32"
    :commits
   '(
;;;                    3a100 3a200 3a300
;;;                   /
;;;                  /
    10000 20000 30000
;;;                  \                         3b4a1 3b4a2
;;;                   \                       /
                       3b100 3b200 3b300 3b400
;;;                                           \
                                               3b4b1 3b4b2 3b4b3 3b4b4 3b4b5
    )
    )

   (list
    :url "https://gitlab.com/debdistutils/guix/mirror"
    :tstp ""
    :commits
   '(
;;;                    3a100 3a200 3a300
;;;                   /
;;;                  /
    10000 20000 30000
;;;                  \                         3b4a1 3b4a2
;;;                   \                       /
                       3b100 3b200 3b300 3b400
;;;                                           \
                                               3b4b1 3b4b2 3b4b3 3b4b4 3b4b5
    )
    )
   (list
    :url "https://https.git.savannah.gnu.org/git/guix.git"
    :finger-print ""
    :tstp "2026-01-12_19-07-32"
    :commits
   '(
;;;                    3a100 3a200 3a300
;;;                   /
;;;                  /
    10000 20000 30000
;;;                  \                         3b4a1 3b4a2
;;;                   \                       /
                       3b100 3b200 3b300 3b400
;;;                                           \
                                               3b4b1 3b4b2 3b4b3 3b4b4 3b4b5
    )
    )

;;; attesters with last shasum made no more than 1 week ago
   (list
    :url "https://codeberg.org/lechner/guix-mirror"
    :finger-print ""
    :tstp "2026-01-12_19-07-32"
    :commits
   '(
;;;                    3a100 3a200 3a300
;;;                   /
;;;                  /
    10000 20000 30000
;;;                  \                         3b4a1 3b4a2
;;;                   \                       /
                       3b100 3b200 3b300 3b400
;;;                                           \
                                               3b4b1 3b4b2 3b4b3 3b4b4 3b4b5
    )
    )
   (list
    :url "https://codeberg.org/fishinthecalculator/guix-mirror"
    :finger-print ""
    :tstp "2026-01-12_19-07-32"
    :commits
   '(
;;;                    3a100 3a200 3a300
;;;                   /
;;;                  /
    10000 20000 30000
;;;                  \                         3b4a1 3b4a2
;;;                   \                       /
                       3b100 3b200 3b300 3b400
;;;                                           \
                                               3b4b1 3b4b2 3b4b3 3b4b4 3b4b5
    )
    )
   (list
    :url "https://codeberg.org/levenson/guix"
    :finger-print ""
    :tstp "2026-01-12_19-07-32"
    :commits
   '(
;;;                    3a100 3a200 3a300
;;;                   /
;;;                  /
    10000 20000 30000
;;;                  \                         3b4a1 3b4a2
;;;                   \                       /
                       3b100 3b200 3b300 3b400
;;;                                           \
                                               3b4b1 3b4b2 3b4b3 3b4b4 3b4b5
    )
    )
   ))
