(define-module (dotf attest)
  #:use-module (dotf utils)
  #:use-module (dotf tests)
  #:use-module (srfi srfi-88)  ; provides keyword objects
  #:use-module (guix channels) ; %default-channels
  #:use-module (srfi srfi-19)  ; string->date
  #:use-module (ice-9 match)
  )

(define (parse-tstp tstp-string)
  "Parse \"YYYY-MM-DD_HH-MM-SS\" into an SRFI-19 date.
Example:
(parse-tstp \"2026-01-12_19-07-32\") ; =>
#<date nanosecond: 0 second: 32 minute: 7 hour: 19 day: 12 month: 1 year: 2026 zone-offset: 3600>
"
  (string->date tstp-string "~Y-~m-~d_~H-~M-~S"))

(define (days-between date-x date-y)
  "Return number of whole days between DATE-X and DATE-Y (both SRFI-19 dates).
Example:
(days-between (parse-tstp \"2026-01-02_00-00-00\")
              (parse-tstp \"2026-01-01_00-00-00\"))
;; => 1"
  (let* ((x-time     (date->time-utc date-x))
         (y-time     (date->time-utc date-y))
         (delta-time (- (time-second x-time) (time-second y-time))))
    ;; (* 24 #| hours |# 60 #| minutes |# 60 #| seconds |#) ; => 86400
    ;; (let ((delta-time 172799)) ;; 1 day, 23 hours, 59 minutes, 59 seconds
    ;;   (format #t "(/ delta-time 86400.0)    : ~a\n(inexact->exact ...) : ~a\n"
    ;;           (/ delta-time 86400.0)
    ;;           (inexact->exact (floor (/ delta-time 86400)))))
    (inexact->exact (floor (/ delta-time 86400)))))

;; --- Example -------------------------------------------------------------

;; TODO add #:owner to every fork and consider increasing the commit score when
;; author, commiter and fork owner differ
;; A single owner can have multiple forks

;; New commits are typically comming-in from the official upstream, this may
;; change however, when the community splits for some reason
(define new-commits
  '(
    (#:sha 3b100 #:commited "2026-01-01_10-00-00" #:author "..." #:commiter "..." #| more keys & vals |#) ; 0
    (#:sha 3b200 #:commited "2026-01-01_11-00-00" #:author "..." #:commiter "..." #| more keys & vals |#) ; 1
    (#:sha 3b300 #:commited "2026-01-01_12-00-00" #:author "..." #:commiter "..." #| more keys & vals |#) ; 2

    (#:sha 3b400 #:commited "2026-01-02_10-00-00" #:author "..." #:commiter "..." #| more keys & vals |#) ; 3
    (#:sha 3b4b1 #:commited "2026-01-02_10-00-00" #:author "..." #:commiter "..." #| more keys & vals |#) ; 4

    (#:sha 3b4b2 #:commited "2026-01-05_08-00-00" #:author "..." #:commiter "..." #| more keys & vals |#) ; 5
    (#:sha 3b4b3 #:commited "2026-01-05_16-00-00" #:author "..." #:commiter "..." #| more keys & vals |#) ; 6

    (#:sha 3b4b4 #:commited "2026-01-08_12-23-00" #:author "..." #:commiter "..." #| more keys & vals |#) ; 7
    (#:sha 3b4b5 #:commited "2026-01-12_00-44-55" #:author "..." #:commiter "..." #| more keys & vals |#) ; 8
    ))

(define fork1
  (list
   (append (list-ref new-commits 0) (list #:attested "2026-01-01_13-00-20" #| more keys & vals |#))
   (append (list-ref new-commits 1) (list #:attested "2026-01-01_13-00-20" #| more keys & vals |#))
   (append (list-ref new-commits 2) (list #:attested "2026-01-01_13-00-20" #| more keys & vals |#))
   (append (list-ref new-commits 3) (list #:attested "2026-01-02_19-00-23" #| more keys & vals |#))
   (append (list-ref new-commits 4) (list #:attested "2026-01-02_19-00-23" #| more keys & vals |#))
   (append (list-ref new-commits 5) (list #:attested "2026-01-06_12-38-01" #| more keys & vals |#))
   (append (list-ref new-commits 6) (list #:attested "2026-01-07_12-40-24" #| more keys & vals |#))
   ))

(define fork2
  (list
   (append (list-ref new-commits 0) (list #:attested "2026-01-01_23-40-00" #| more keys & vals |#))
   (append (list-ref new-commits 1) (list #:attested "2026-01-01_23-40-00" #| more keys & vals |#))
   (append (list-ref new-commits 2) (list #:attested "2026-01-01_23-40-00" #| more keys & vals |#))
   (append (list-ref new-commits 3) (list #:attested "2026-01-04_09-11-00" #| more keys & vals |#))
   (append (list-ref new-commits 4) (list #:attested "2026-01-04_09-11-00" #| more keys & vals |#))
   (append (list-ref new-commits 5) (list #:attested "2026-01-10_09-15-33" #| more keys & vals |#))
   (append (list-ref new-commits 6) (list #:attested "2026-01-10_09-15-33" #| more keys & vals |#))
   (append (list-ref new-commits 7) (list #:attested "2026-01-10_09-15-33" #| more keys & vals |#))
   ))

(define fork3
  (list
   (append (list-ref new-commits 0) (list #:attested "2026-01-10_09-58-07" #| more keys & vals |#))
   (append (list-ref new-commits 1) (list #:attested "2026-01-10_09-58-07" #| more keys & vals |#))
   (append (list-ref new-commits 2) (list #:attested "2026-01-10_09-58-07" #| more keys & vals |#))
   (append (list-ref new-commits 3) (list #:attested "2026-01-10_09-58-07" #| more keys & vals |#))
   (append (list-ref new-commits 4) (list #:attested "2026-01-10_09-58-07" #| more keys & vals |#))
   (append (list-ref new-commits 5) (list #:attested "2026-01-10_09-58-07" #| more keys & vals |#))
   (append (list-ref new-commits 6) (list #:attested "2026-01-10_09-58-07" #| more keys & vals |#))
   ))

;; Define a scoring function over incomming commits, computed from observable
;; signals (commit-age, adoption, stability, divergence, etc.), then map that
;; score to a probability-like number in [0,1]. Then pick the newest upstream
;; commit whose probability ≥ 0.5.
;;

;; --- small utils ---------------------------------------------------------

(define (clamp x lo hi)
  "Force X to stay between LO and HI. (Clamp - wood working tool.)
Examples:
(clamp 1 2 3) => 2
(clamp 5 1 9) => 5"
  (max lo (min hi x)))

(define (logistic x)
  "Sigmoid / logistic curve https://en.wikipedia.org/wiki/Logistic_function
It maps any real number to a number strictly between 0 and 1:
- Very negative x → result close to 0
- x = 0 → result exactly 0.5
- Very positive x → result close to 1
It turns an unbounded score into something that behaves like a probability."
  (/ 1.0 (+ 1.0 (exp (- x)))))

;; --- indexing attesters ------------------------------------------------------
(define (find-by-sha commits sha)
  "Return the first alist in COMMITS whose #:sha key matches SHA.
(find-by-sha fork1 '3b200)
;; =>
(#:sha #{3b200}# #:commited \"2026-01-01_11-00-00\" #:attested \"2026-01-01_13-00-20\")
"
  (let ((results
         (filter (lambda (commit) (eq? sha (plist-get commit #:sha)))
                 commits)))
    (cond
     [(= 1 (length results)) (car results)]
     [(= 0 (length results)) #f]
     ;; [else] ;; TODO raise an exception
     )))

;; You will likely want a 'mirror?' classifier later; for now treat all
;; attesters as human.
(define (fork-human? fork) #t)

;; --- feature extraction --------------------------------------------------

(define (commit-age-since date commit)
  "Number of days between a COMMIT and a DATE. (Age of COMMIT.)
Example:
(define commit '(#:commited \"2026-01-01_00-00-00\"))
(commit-age-since (parse-tstp \"2026-01-02_00-00-00\") commit) ; => 1"
  (days-between date (parse-tstp (plist-get commit #:commited))))

(define (attest-age-since date attest)
  "Number of days between an ATTEST and a DATE. (Age of ATTEST.)
Example:
(define attest '(#:attested \"2026-01-01_00-00-00\"))
(attest-age-since (parse-tstp \"2026-01-02_00-00-00\") attest) ; => 1"
  (days-between date (parse-tstp (plist-get attest #:attested))))

(define (count-mature-attesters
         since-date
         min-days
         attestation-repos
         sha
         )
  "Count of human-made attestation-repositories in which the SHA is present for
at least MIN-DAYS since the SINCE-DATE."
  (let loop ((attestation-repos attestation-repos)
             (cnt-sha-occurencies 0))
    (cond
      ((null? attestation-repos) cnt-sha-occurencies)
      (else
       (let* ((attestation-repo (car attestation-repos))
              (attested-commit (find-by-sha attestation-repo sha)))
         (loop (cdr attestation-repos)
               (if (and (fork-human? attestation-repo)
                        attested-commit ;; does an attested-commit even exist?
                        (>= (attest-age-since since-date attested-commit) min-days))
                   (1+ cnt-sha-occurencies)
                   cnt-sha-occurencies)))))))

;; Distance/behind computation needs commit graph; for now approximate by list index.
(define (sha-index commits sha)
  "(sha-index '((#:sha a) (#:sha b) (#:sha c)) 'b) => 1"
  (let loop ((loop-commits commits) (idx 0))
    (cond ((null? loop-commits) #f)
          ((eq? (shasum (car loop-commits)) sha) idx)
          (else (loop (cdr loop-commits) (1+ idx))))))

(define (shasum commit) (plist-get commit #:sha))

(define (distance-between commits sha-x sha-y)
  "Distance (i.e. number of commits) between the SHA-X and SHA-Y.
Example:
(define commits '((#:sha a) (#:sha b) (#:sha c)))
(distance-between commits 'a 'a) ; => 0
(distance-between commits 'a 'b) ; => 1
(distance-between commits 'a 'c) ; => 2"
  (let ((idx-x (sha-index commits sha-x))
        (idx-y (sha-index commits sha-y)))
    (and idx-x idx-y (max 0 (- idx-y idx-x)))))

;; --- scoring -------------------------------------------------------------

(define (feature-age days)
  "How good/bad the age of the commit is. Newer than ~2 days is negative; older
 is positive; saturates.
Examples:
(feature-age 0)  ; => -0.2
(feature-age 1)  ; => -0.1
(feature-age 2)  ; =>  0.0
(feature-age 3)  ; =>  0.1
(feature-age 4)  ; =>  0.2
(feature-age 5)  ; =>  0.3
(feature-age 6)  ; =>  0.4
(feature-age 7)  ; =>  0.5
(feature-age 8)  ; =>  0.6
(feature-age 9)  ; =>  0.7
(feature-age 10) ; =>  0.8
(feature-age 11) ; =>  0.9
(feature-age 12) ; =>  1.0"
  (clamp (/ (- days 2.0) 10.0)
         -1.0 1.0))

(define (feature-adopt nr-of-adopters)
  "How good/bad the mature adoption is. Need at least a handful of independent
humans.
Examples:
(feature-adopt 0)  ; => -0.3
(feature-adopt 1)  ; => -0.2
(feature-adopt 2)  ; => -0.1
(feature-adopt 3)  ; =>  0.0
(feature-adopt 4)  ; =>  0.1
(feature-adopt 5)  ; =>  0.2
(feature-adopt 6)  ; =>  0.3
(feature-adopt 7)  ; =>  0.4
(feature-adopt 8)  ; =>  0.5
(feature-adopt 9)  ; =>  0.6
(feature-adopt 10) ; =>  0.7
(feature-adopt 11) ; =>  0.8
(feature-adopt 12) ; =>  0.9
(feature-adopt 13) ; =>  1.0"
  (clamp (/ (- nr-of-adopters 3.0) 10.0)
         -1.0 1.0))

(define (feature-behind nr-of-commits)
  "How strong the pressure to update is. Being behind increases benefit,
saturates.
Examples:
(feature-behind 0)   ; => 0.0
(feature-behind 1)   ; => 0.02
(feature-behind 2)   ; => 0.04
(feature-behind 3)   ; => 0.06
(feature-behind 4)   ; => 0.08
(feature-behind 5)   ; => 0.10
(feature-behind 6)   ; => 0.12
(feature-behind 7)   ; => 0.14
(feature-behind 8)   ; => 0.16
(feature-behind 9)   ; => 0.18
(feature-behind 10)  ; => 0.20
(feature-behind 11)  ; => 0.22
(feature-behind 12)  ; => 0.24
(feature-behind 13)  ; => 0.26
(feature-behind 14)  ; => 0.28
(feature-behind 15)  ; => 0.30
(feature-behind 20)  ; => 0.40
(feature-behind 25)  ; => 0.50
(feature-behind 30)  ; => 0.60
(feature-behind 40)  ; => 0.80
(feature-behind 50)  ; => 1.00"
  (clamp (/ nr-of-commits 50.0)
         0.0 1.0))

(define (feature-churn nr-of-commits-over-nr-of-days)
  "Churn rate. Churn = sčerenie
High churn rate:
  Less time for testing individual commits, higher chance of breakage.
Low churn rate:
  More time for testing individual commits, lower integration risk.

L - current local commit
C - candidate upstream commit C

                  Nr of commits between L and C
churn_rate(L,C)= --------------------------------	​
                  Nr of days between of L and C

30.0 is a normalization constant:
  \"30 commits per day is already extreme, saturate there\"

(feature-churn (/ 50 200)) ; => 0.008333333333333333 ; 50 commits in 200 days
(feature-churn (/ 30 2))   ; => 0.5
(feature-churn (/ 50 2))   ; => 0.8333333333333334   ; 50 commits in 2 days
(feature-churn (/ 30 1))   ; => 1.0"
  (clamp (/ nr-of-commits-over-nr-of-days 30.0)
         0.0 1.0))

(define* (profitability-score
          curr-commit  ; current local commit
          cand-commit  ; adoption candidate
          new-commits  ; incomming commits from the upstream
          attestation-repos
          #:key
          (since-date (current-date))
          (min-maturity-days 5)
          (weight-age 1.0)
          (weight-adopt 1.0)
          (weight-behind 1.0))
  "Interpret each feature as adding/subtracting from a score.

High WEIGHT-AGE and high WEIGHT-ADOPT values:
  Conservative adoption policy. (You care a lot about maturity and wide use)
High WEIGHT-BEHIND value:
  Avoid being too far behind the upstream (security fixes)
Low WEIGHT-BEHIND value:
  Almost ignore how far behind the upstream you are."

  (let* ((curr-sha (shasum curr-commit))
         (cand-sha (shasum cand-commit))
         (cand-age (commit-age-since since-date cand-commit))
         (mature-attesters (count-mature-attesters
                            since-date
                            min-maturity-days
                            attestation-repos
                            cand-sha))
         (commit-distance (distance-between new-commits curr-sha cand-sha)))
    (+ (* weight-age    (feature-age cand-age))
       (* weight-adopt  (feature-adopt mature-attesters))
       (* weight-behind (feature-behind (or commit-distance 0))))))

;; --- decision ------------------------------------------------------------

(define* (find-newest-profitable-commit
          curr-commit new-commits attestation-repos
          #:key (min-profitability 0.5))
  "Return the newest COMMIT whose profitability is at least MIN-PROFITABILITY,
calculated using ATTESTATION-REPOS."
  (let loop ((candidate-commits (reverse new-commits)))
    (cond
      ((null? candidate-commits) #f)
      (else
       (let* ((cand-commit (car candidate-commits))
              (cand-score (profitability-score
                                   curr-commit
                                   cand-commit
                                   new-commits
                                   attestation-repos
                                   )))
         (if (>= (logistic cand-score) min-profitability)
             cand-commit
             (loop (cdr candidate-commits))))))))

;; 2) The biggest conceptual issue: maturity should use fork timestamp, not upstream commit timestamp
;; You got this right (you use #:attested). Good.
;; But your candidate selection currently does not require “commit exists in all forks”. That’s OK if your profitability model treats “not present” as “not mature” (it currently does). Just be sure that’s what you want: a commit with 0 attestations can still pass if weight-behind is large. If that’s not desired, add a hard gate or a penalty.
