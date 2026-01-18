(define-module (dotf attest)
  #:use-module (srfi srfi-1)   ; list-processing procedures
  #:use-module (srfi srfi-19)  ; string->date
  #:use-module (srfi srfi-88)  ; provides keyword objects
  )

;; --- Example -------------------------------------------------------------

;; TODO add #:owner to every fork and consider increasing the commit score when
;; author, commiter and fork owner differ. Also a single owner can have multiple
;; forks

;; New commits are typically comming from the official upstream.
(define new-commits
  '(
    (#:sha 3b100 #:commited "2026-01-01_10-00-00" #:author (#:name "..." #:finger-print "...") #:commiter (:#name "..." #:finger-print "...")  #| more keys & vals |#) ; 0
    (#:sha 3b200 #:commited "2026-01-01_11-00-00" #:author (#:name "..." #:finger-print "...") #:commiter (:#name "..." #:finger-print "...")  #| more keys & vals |#) ; 1
    (#:sha 3b300 #:commited "2026-01-01_12-00-00" #:author (#:name "..." #:finger-print "...") #:commiter (:#name "..." #:finger-print "...")  #| more keys & vals |#) ; 2
    (#:sha 3b400 #:commited "2026-01-02_10-00-00" #:author (#:name "..." #:finger-print "...") #:commiter (:#name "..." #:finger-print "...")  #| more keys & vals |#) ; 3
    (#:sha 3b4b1 #:commited "2026-01-02_10-00-00" #:author (#:name "..." #:finger-print "...") #:commiter (:#name "..." #:finger-print "...")  #| more keys & vals |#) ; 4
    (#:sha 3b4b2 #:commited "2026-01-05_08-00-00" #:author (#:name "..." #:finger-print "...") #:commiter (:#name "..." #:finger-print "...")  #| more keys & vals |#) ; 5
    (#:sha 3b4b3 #:commited "2026-01-05_16-00-00" #:author (#:name "..." #:finger-print "...") #:commiter (:#name "..." #:finger-print "...")  #| more keys & vals |#) ; 6
    (#:sha 3b4b4 #:commited "2026-01-08_12-23-00" #:author (#:name "..." #:finger-print "...") #:commiter (:#name "..." #:finger-print "...")  #| more keys & vals |#) ; 7
    (#:sha 3b4b5 #:commited "2026-01-12_00-44-55" #:author (#:name "..." #:finger-print "...") #:commiter (:#name "..." #:finger-print "...")  #| more keys & vals |#) ; 8
    ))

(define fork1
  (list
   (append (list-ref new-commits 0) '(#:attested "2026-01-01_13-00-20" #| more keys & vals |#))
   (append (list-ref new-commits 1) '(#:attested "2026-01-01_13-00-20" #| more keys & vals |#))
   (append (list-ref new-commits 2) '(#:attested "2026-01-01_13-00-20" #| more keys & vals |#))
   (append (list-ref new-commits 3) '(#:attested "2026-01-02_19-00-23" #| more keys & vals |#))
   (append (list-ref new-commits 4) '(#:attested "2026-01-02_19-00-23" #| more keys & vals |#))
   (append (list-ref new-commits 5) '(#:attested "2026-01-06_12-38-01" #| more keys & vals |#))
   (append (list-ref new-commits 6) '(#:attested "2026-01-07_12-40-24" #| more keys & vals |#))
   ))

(define fork2
  (list
   (append (list-ref new-commits 0) '(#:attested "2026-01-01_23-40-00" #| more keys & vals |#))
   (append (list-ref new-commits 1) '(#:attested "2026-01-01_23-40-00" #| more keys & vals |#))
   (append (list-ref new-commits 2) '(#:attested "2026-01-01_23-40-00" #| more keys & vals |#))
   (append (list-ref new-commits 3) '(#:attested "2026-01-04_09-11-00" #| more keys & vals |#))
   (append (list-ref new-commits 4) '(#:attested "2026-01-04_09-11-00" #| more keys & vals |#))
   (append (list-ref new-commits 5) '(#:attested "2026-01-10_09-15-33" #| more keys & vals |#))
   (append (list-ref new-commits 6) '(#:attested "2026-01-10_09-15-33" #| more keys & vals |#))
   (append (list-ref new-commits 7) '(#:attested "2026-01-10_09-15-33" #| more keys & vals |#))
   ))

(define fork3
  (list
   (append (list-ref new-commits 0) '(#:attested "2026-01-10_09-58-07" #| more keys & vals |#))
   (append (list-ref new-commits 1) '(#:attested "2026-01-10_09-58-07" #| more keys & vals |#))
   (append (list-ref new-commits 2) '(#:attested "2026-01-10_09-58-07" #| more keys & vals |#))
   (append (list-ref new-commits 3) '(#:attested "2026-01-10_09-58-07" #| more keys & vals |#))
   (append (list-ref new-commits 4) '(#:attested "2026-01-10_09-58-07" #| more keys & vals |#))
   (append (list-ref new-commits 5) '(#:attested "2026-01-10_09-58-07" #| more keys & vals |#))
   (append (list-ref new-commits 6) '(#:attested "2026-01-10_09-58-07" #| more keys & vals |#))
   ))

;; Define a scoring function over incomming commits, computed from observable
;; signals (commit-age, adoption, stability, divergence, etc.), then map that
;; score to a probability-like number in [0,1]. Then pick the newest upstream
;; commit whose probability ≥ 0.5.
;;

;; --- small utils ---------------------------------------------------------

(define (partial fun . args)
  "Alternative implementation:
(use-modules (srfi srfi-26))
(map (cut * 2 <>) '(1 2 3 4)) ;; => (2 4 6 8)"
  (lambda x (apply fun (append args x))))

(define (comp . fns)
  "Like `compose'. Can be called with zero arguments. I.e. (thunk? comp) => #t
Works also for functions returning and accepting multiple values."
  (lambda args
    (if (null? fns)
        (apply values args)
        (let [(proc (car fns)) (rest (cdr fns))]
          (if (null? rest)
              (apply proc args)
              (let ((g (apply comp rest)))
                (call-with-values (lambda () (apply g args)) proc)))))))

(define (unspecified-or-empty-or-false? obj)
  (or (unspecified? obj)
      (null? obj)
      (and (string? obj) (string-null? obj))
      (eq? #f obj)))

(define (get-keys lst)
  "Return a list of all keys in the list LST, which may or may not be a plist.
(get-keys '(#:a 1 b 2))   ; => (#:a b)
(get-keys '(a 1 b 2))     ; => (a b)
(get-keys '())            ; => ()
(get-keys '(#:a 1 #:a 3)) ; => (#:a #:a) ; not checking for duplicate keys
(get-keys '(#:a 1 b))     ; => (#:a)
(get-keys 1)              ; => not a list"
  (unless (list? lst)
    (error (format #f "get-keys: `~s' is not a list\n" lst)))

  (let loop ((xs lst) (acc '()))
    (cond
     ((or (null? xs) (null? (cdr xs)))
      (reverse acc))
     (else
      (loop (cddr xs) (cons (car xs) acc))))))

(define (has-duplicates? lst)
  "Used in `plist-unique?'
(has-duplicates? '())        ; => #f
(has-duplicates? '(1 2 3 4)) ; => #f
(has-duplicates? '(1 2 3 2)) ; => #t
(has-duplicates? '(a 1 a 2)) ; => #t
"
  (cond
   ((null? lst) #f)
   ((member (car lst) (cdr lst)) #t)
   (else (has-duplicates? (cdr lst)))))

(define (plist-unique? lst)
  "Empty list is also a plist. Plist must not contain duplicate keys.

TODO Git-commit-like metadata may repeat keys (trailers, parents, etc.); in that
case use a different representation or a non-unique plist predicate. When
addressing this, use `plist?' instead of this procedure in the `plist-get'.

(plist-unique? '(a 1 b 2)) ; => #t
(plist-unique? '())        ; => #t
(plist-unique? '(1))       ; => #f ; odd number of elements
(plist-unique? '(1 2 3))   ; => #f ; odd number of elements
(plist-unique? 1)          ; => #f ; not a list
(plist-unique? '(a 1 a 2)) ; => #f ; 'a is a duplicate key
"
  (and (list? lst) (even? (length lst))
       (not (has-duplicates? (get-keys lst)))))

(define (plist? lst)
  "Shape check only: proper list of even length.

(plist? '(a 1 b 2)) ; => #t
(plist? '())        ; => #t
(plist? '(1))       ; => #f ; odd number of elements
(plist? '(1 2 3))   ; => #f ; odd number of elements
(plist? 1)          ; => #f ; not a list
(plist? '(a 1 a 2)) ; => #t
"
  (and (list? lst) (even? (length lst))))

(define (plist-get . args)
  "Smart plist-get that works with arguments in either order.
(plist-get '(#:y 2 #:x 1) #:x)      ; => 1
(plist-get #:x (list #:y 2 #:x 1))  ; => 1
(plist-get '(#:x 1 #:x 2) #:x)      ; plist-get: expected even-length ...
(plist-get '(#:y 2 #:x 1) #:z)      ; => #f
(plist-get '() #:x)                 ; => #f

(plist-get '(1 11 2 22) 1)          ; => 11
(plist-get '((1 2) 11 2 22) '(1 2)) ; => 11

(plist-get '(42 #:y 2 #:x 1) #:x)   ; plist-get: expected even-length ...

(plist-get)                         ; plist-get: expected exactly ...
(plist-get 1)                       ; plist-get: expected exactly ...
(plist-get '())                     ; plist-get: expected exactly ...
"
  (define (loop plist key)
    (cond [(null? plist) #f]
          [(eq? (car plist) key) (cadr plist)]
          [else (loop (cddr plist) key)]))

  (unless (= 2 (length args))
    (error "plist-get: expected exactly 2 arguments (plist key) or (key plist)"
           args))

  (let* ((loop-args (if (list? (car args))
                        args
                        (reverse args)))
         (plist (car loop-args)))
    (if (plist-unique? plist)
        (apply loop loop-args)
        (error
         "plist-get: expected even-length list of unique key/value pairs"
         plist))))

(define (parse-tstp tstp-string)
  "Parse \"YYYY-MM-DD_HH-MM-SS\" into an SRFI-19 date.
Example:
(parse-tstp \"2026-01-12_19-07-32\") ; =>
#<date nanosecond: 0 second: 32 minute: 7 hour: 19 day: 12 month: 1 year: 2026 zone-offset: 3600>
"
  (string->date tstp-string "~Y-~m-~d_~H-~M-~S"))

(define (days-between begin end)
  "Return number of whole days between BEGIN and END (both SRFI-19 dates).
Examples:
(days-between (parse-tstp \"2026-01-01_00-00-00\")
              (parse-tstp \"2026-01-10_00-00-00\")) ; => 9
"
  (let* ((beg-time     (date->time-utc begin))
         (end-time     (date->time-utc end))
         (delta-time (- (time-second end-time) (time-second beg-time))))
    ;; (* 24 #| hours |# 60 #| minutes |# 60 #| seconds |#) ; => 86400
    ;; (let ((delta-time 172799)) ;; 1 day, 23 hours, 59 minutes, 59 seconds
    ;;   (format #t "(/ delta-time 86400.0)    : ~a\n(inexact->exact ...) : ~a\n"
    ;;           (/ delta-time 86400.0)
    ;;           (inexact->exact (floor (/ delta-time 86400)))))
    (inexact->exact (floor (/ delta-time 86400)))))

(define (clamp x lo hi)
  "Force X to stay between LO and HI. (Clamp - wood working tool.)
Examples:
(clamp 1 2 3) => 2
(clamp 5 1 9) => 5
"
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
  "Return the first commit plist in COMMITS whose #:sha matches SHA, else #f.
Examples:
(find-by-sha fork1 '3b200x) ; => #f
(find-by-sha fork1 '3b200)  ; => (#:sha #{3b200}# ...)
"
  (let loop ((xs commits))
    (cond ((null? xs) #f)
          ((eq? sha (shasum (car xs))) (car xs))
          (else (loop (cdr xs))))))

;; You will likely want a 'mirror?' classifier later; for now treat all
;; attesters as human.
(define (fork-human? fork) #t)

;; --- feature extraction --------------------------------------------------

(define (shasum commit) (plist-get commit #:sha))

(define (commit-age until-date commit)
  "Number of days between a COMMIT and a UNTIL-DATE (SRFI-19). Age of COMMIT.
Examples:
(define commit '(#:commited \"2026-01-01_00-00-00\"))
(commit-age (parse-tstp \"2026-01-02_00-00-00\") commit) ; => 1
(commit-age (parse-tstp \"2025-12-31_00-00-00\") commit) ; => -1
"
  (days-between (parse-tstp (plist-get commit #:commited)) until-date))

(define (attest-age until-date commit)
  "Number of days between an COMMIT and a UNTIL-DATE (SRFI-19). Age of ATTEST.
Examples:
(define commit '(#:attested \"2026-01-01_00-00-00\"))
(attest-age (parse-tstp \"2026-01-02_00-00-00\") commit) ; => 1
(attest-age (parse-tstp \"2025-12-31_00-00-00\") commit) ; => -1
"
  (days-between (parse-tstp (plist-get commit #:attested)) until-date))

(define (count-mature-attesters date min-days attestation-repos sha)
  "Count human-made repositories where SHA exists and is attested at least
 MIN-DAYS until DATE (SRFI-19).
Examples:
(define date (parse-tstp \"2026-01-11_00-00-00\"))
(define attestation-repo
 '((#:sha a #:attested \"2026-01-02_00-00-00\")
   (#:sha b #:attested \"2026-01-02_00-00-00\")
   (#:sha c #:attested \"2026-01-02_00-00-00\")))
(count-mature-attesters date 5 (list attestation-repo) 'b) ; => 1
(count-mature-attesters date 5 (list attestation-repo) 'x) ; => 0
"
  (let loop ((repos attestation-repos)
             (count 0))
    (if (null? repos)
        count
        (let* ((repo (car repos))
               (commit (find-by-sha repo sha)))
          (loop (cdr repos)
                (if (and commit (fork-human? repo)
                         (>= (attest-age date commit) min-days))
                    (+ count 1)
                    count))))))

(define (sha-index commits sha)
  "0-based index of SHA in COMMITS, or #f.
Examples:
(sha-index '((#:sha a) (#:sha b) (#:sha c)) 'x) ; => #f
(sha-index '((#:sha a) (#:sha b) (#:sha c)) 'b) ; => 1
"
  (let loop ((xs commits) (idx 0))
    (cond ((null? xs) #f)
          ((eq? (shasum (car xs)) sha) idx)
          (else (loop (cdr xs) (+ idx 1))))))

;; Distance/behind computation needs commit graph; for now approximate by list index.
(define (distance-between commits sha-x sha-y)
  "Distance in number-of-commits from SHA-X to SHA-Y, assuming COMMITS is
oldest->newest.
Examples:
(distance-between '((#:sha a) (#:sha b) (#:sha c)) 'a 'a) ; => 0
(distance-between '((#:sha a) (#:sha b) (#:sha c)) 'a 'b) ; => 1
(distance-between '((#:sha a) (#:sha b) (#:sha c)) 'a 'c) ; => 2
(distance-between '((#:sha a) (#:sha b) (#:sha c)) 'a 'x) ; => #f
(distance-between '((#:sha a) (#:sha b) (#:sha c)) 'x 'y) ; => #f
(distance-between '((#:sha a) (#:sha b) (#:sha c)) 'x 'c) ; => #f
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
          new-commits  ; incomming commits from the upstream
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
(define curr-commit '(#:sha 3b000 #:commited \"2026-01-01_9-00-00\"))
(define attestation-repos (list fork1 fork2 fork3))
(find-newest-profitable-commit curr-commit new-commits attestation-repos)
;; => (#:sha #{3b4b3}# #:commited "2026-01-05_16-00-00" ...)
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
