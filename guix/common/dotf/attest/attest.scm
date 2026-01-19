(define-module (dotf attest attest)
  #:use-module (dotf attest data)
  #:use-module (dotf attest utils)
  #:use-module (srfi srfi-1)   ; list-processing procedures
  #:use-module (srfi srfi-19)  ; current-date
  #:use-module (srfi srfi-88)  ; provides keyword objects
  )

(read-set! keywords 'prefix) ; Allow both :keyword and #:keyword

;; Define a scoring function over incoming commits, computed from observable
;; signals (commit-age, adoption, stability, divergence, etc.), then map that
;; score to a probability-like number in [0,1]. Then pick the newest upstream
;; commit whose probability ≥ 0.5.

(define (find-by-sha commits sha)
  "Return the first commit plist in COMMITS whose shasum matches SHA, else #f.
Examples:
(find-by-sha fork1 '3b200x) ; => #f
(find-by-sha fork1 '3b200)  ; => (list ksha '3b200 kcommitted \"...\" ...)
"
  (let loop ((xs commits))
    (cond ((null? xs) #f)
          ((eq? sha (shasum (car xs))) (car xs))
          (else (loop (cdr xs))))))

;; You will likely want a 'mirror?' classifier later; for now treat all
;; attesters as human.
(define (fork-human? fork) #t)

;; --- feature extraction --------------------------------------------------

(define (shasum commit) (unique-plist-get commit ksha))

(define (commit-age until-date commit)
  "Number of days between a COMMIT and a UNTIL-DATE (SRFI-19). Age of COMMIT.
Examples:
(define commit (list kcommitted \"2026-01-01_00-00-00\"))
(commit-age \"2025-12-31_00-00-00\" commit)    ; => (forgot to use parse-tstp)
(commit-age (parse-tstp \"2026-01-02_00-00-00\") commit) ; => 1
(commit-age (parse-tstp \"2025-12-31_00-00-00\") commit) ; => -1
(commit-age (parse-tstp \"2026-01-02_00-00-00\") (list)) ; => #f
(commit-age (parse-tstp \"2026-01-02_00-00-00\") (list kcommitted)) ; => error
"
  (let ((ts (unique-plist-get commit kcommitted)))
    (and ts (days-between (parse-tstp ts) until-date))))

(define (attest-age until-date commit)
  "Number of days between an COMMIT and a UNTIL-DATE (SRFI-19). Age of ATTEST.
Examples:
(define commit (list kattested \"2026-01-01_00-00-00\"))
(attest-age \"2025-12-31_00-00-00\" commit)    ; => (forgot to use parse-tstp)
(attest-age (parse-tstp \"2026-01-02_00-00-00\") commit) ; => 1
(attest-age (parse-tstp \"2025-12-31_00-00-00\") commit) ; => -1
(attest-age (parse-tstp \"2026-01-02_00-00-00\") (list)) ; => #f
(attest-age (parse-tstp \"2026-01-02_00-00-00\") (list kattested)) ; => error
"
  (let ((ts (unique-plist-get commit kattested)))
    (and ts (days-between (parse-tstp ts) until-date))))

(define (count-mature-attesters until-date min-days attestation-repos sha)
  "Count human-made repositories where SHA exists and is attested at least
 MIN-DAYS as of UNTIL-DATE (SRFI-19).
Examples:
(define until-date (parse-tstp \"2026-01-11_00-00-00\"))
(define attestation-repo
 '((ksha a kattested \"2026-01-02_00-00-00\")
   (ksha b kattested \"2026-01-02_00-00-00\")
   (ksha c kattested \"2026-01-02_00-00-00\")))
(count-mature-attesters until-date 5 (list attestation-repo) 'b) ; => 1
(count-mature-attesters until-date 5 (list attestation-repo) 'x) ; => 0
"
  (let loop ((repos attestation-repos)
             (count 0))
    (if (null? repos)
        count
        (let* ((repo (car repos))
               (commit (find-by-sha repo sha)))
          (loop (cdr repos)
                (if (and commit (fork-human? repo)
                         (let ((age (attest-age until-date commit)))
                           (and age (>= age min-days))))
                    (1+ count)
                    count))))))

(define (sha-index commits sha)
  "0-based index of SHA in COMMITS, or #f.
Examples:
(sha-index '((ksha a) (ksha b) (ksha c)) 'x) ; => #f
(sha-index '((ksha a) (ksha b) (ksha c)) 'b) ; => 1
"
  (let loop ((xs commits) (idx 0))
    (cond ((null? xs) #f)
          ((eq? (shasum (car xs)) sha) idx)
          (else (loop (cdr xs) (1+ idx))))))

;; Distance/behind computation needs commit graph; for now approximate by list
;; index.
(define (distance-between commits sha-x sha-y)
  "Distance in number-of-commits from SHA-X to SHA-Y, assuming COMMITS is
oldest->newest.
Examples:
(distance-between '((ksha a) (ksha b) (ksha c)) 'a 'a) ; => 0
(distance-between '((ksha a) (ksha b) (ksha c)) 'a 'b) ; => 1
(distance-between '((ksha a) (ksha b) (ksha c)) 'a 'c) ; => 2
(distance-between '((ksha a) (ksha b) (ksha c)) 'a 'x) ; => #f
(distance-between '((ksha a) (ksha b) (ksha c)) 'x 'y) ; => #f
(distance-between '((ksha a) (ksha b) (ksha c)) 'x 'c) ; => #f
"
  (let ((ix (sha-index commits sha-x))
        (iy (sha-index commits sha-y)))
    (and ix iy (max 0 (- iy ix)))))

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

(define (feature-adopter-gate n min-n)
  "Returns 0 when n >= min-n, negative otherwise, saturating at -1.
Examples:
(feature-adopter-gate 0 3) ; => -1.0
(feature-adopter-gate 1 3) ; => -0.6666
(feature-adopter-gate 2 3) ; => -0.3333
(feature-adopter-gate 3 3) ; =>  0.0
(feature-adopter-gate 4 3) ; =>  0.0
(feature-adopter-gate 5 3) ; =>  0.0
"
  (clamp (/ (- n min-n) (max 1.0 min-n))
         -1.0 0.0))

(define* (profitability-score
          curr-commit  ; current local commit
          cand-commit  ; adoption candidate
          new-commits  ; incoming commits from the upstream
          attestation-repos
          #:key
          (until-date (current-date)) ; SRFI-19 date
          (min-maturity-days 5) ; minimal maturity for the adoption candidate
          (min-adopters-count 3)
          ;; returning gate-penalty ensures `(logistic ...)` approaches 0
          (gate-penalty -100.0)
          (weight-age 1.0)
          (weight-adopt 1.0)
          (weight-behind 1.0))
  "Interpret each feature as adding/subtracting from a score.

High WEIGHT-AGE and high WEIGHT-ADOPT values:
  Conservative adoption policy. (You care a lot about maturity and wide use)
High WEIGHT-BEHIND value:
  Avoid being too far behind the upstream (security fixes)
Low WEIGHT-BEHIND value:
  Almost ignore how far behind the upstream you are.
"
  (let* ((curr-sha (shasum curr-commit))
         (cand-sha (shasum cand-commit))
         (cand-age (commit-age until-date cand-commit))
         (mature-attesters (count-mature-attesters
                            until-date
                            min-maturity-days
                            attestation-repos
                            cand-sha))
         (commit-distance (distance-between new-commits curr-sha cand-sha)))
    ;; Rule of thumb:
    ;; - For a policy constraint ("never update unless ≥ N attestations"), use
    ;;   some form of an if-then-else hard gate.
    ;; - For a heuristic ("prefer N, but sometimes accept fewer if I'm far
    ;;   behind"), use a soft gate (e.g. `feature-adopter-gate`) together with
    ;;   a big `weight-gate` in:
    ;;      (+ ... (* weight-gate (feature-adopter-gate ...)))
    (if (< mature-attesters min-adopters-count)
        gate-penalty
        (+ (* weight-age    (feature-age cand-age))
           (* weight-adopt  (feature-adopt mature-attesters))
           (* weight-behind (feature-behind (or commit-distance 0)))))))

;; --- decision ------------------------------------------------------------

(define* (find-newest-profitable-commit
          curr-commit new-commits attestation-repos
          #:key (min-profitability 0.5))
  "Return the newest COMMIT whose profitability is at least MIN-PROFITABILITY,
calculated using ATTESTATION-REPOS.
Examples:
(define curr-commit (list ksha '3b000 kcommitted \"2026-01-01_9-00-00\"))
(define attestation-repos (list fork1 fork2 fork3))
(find-newest-profitable-commit curr-commit new-commits attestation-repos)
;; => (...) ; returns a valid result
"
  (let loop ((candidate-commits (reverse new-commits)))
    (cond
     ((null? candidate-commits) #f)
     (else
      (let* ((cand-commit (car candidate-commits))
             (cand-score (profitability-score
                          curr-commit
                          cand-commit
                          new-commits
                          attestation-repos)))
        (if (>= (logistic cand-score) min-profitability)
            cand-commit
            (loop (cdr candidate-commits))))))))
