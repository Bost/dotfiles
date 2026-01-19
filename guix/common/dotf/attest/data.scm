(define-module (dotf attest data)
  #:use-module (srfi srfi-1)   ; list-processing procedures
  #:use-module (srfi srfi-19)  ; current-date
  #:use-module (srfi srfi-88)  ; provides keyword objects
  #:export
  (
   ksha
   kcommitted
   kauthor
   kattested
   kname
   kcommitter
   kfinger-print

   new-commits
   fork1
   fork2
   fork3
   ))

(read-set! keywords 'prefix) ; Allow both :keyword and #:keyword

;; --- Example -------------------------------------------------------------

;; TODO add :owner to every fork and consider increasing the commit score when
;; author, committer and fork owner differ. Also a single owner can have
;; multiple forks

(define ksha :s)
(define kcommitted :tc)
(define kauthor :a)
(define kattested :ta)
(define kname :n)
(define kcommitter :c)
(define kfinger-print :f)

(define (person name fp) (list kname name kfinger-print fp))

;; New commits are typically coming from the official upstream.
(define new-commits
  (list
   (list ksha '3b100 kcommitted "2026-01-01_10-00-00" kauthor (person "name" "fp") kcommitter (person "name" "fp") #| more k&v |#) ; 0
   (list ksha '3b200 kcommitted "2026-01-01_11-00-00" kauthor (person "name" "fp") kcommitter (person "name" "fp") #| more k&v |#) ; 1
   (list ksha '3b300 kcommitted "2026-01-01_12-00-00" kauthor (person "name" "fp") kcommitter (person "name" "fp") #| more k&v |#) ; 2
   (list ksha '3b400 kcommitted "2026-01-02_10-00-00" kauthor (person "name" "fp") kcommitter (person "name" "fp") #| more k&v |#) ; 3
   (list ksha '3b4b1 kcommitted "2026-01-02_10-00-00" kauthor (person "name" "fp") kcommitter (person "name" "fp") #| more k&v |#) ; 4
   (list ksha '3b4b2 kcommitted "2026-01-05_08-00-00" kauthor (person "name" "fp") kcommitter (person "name" "fp") #| more k&v |#) ; 5
   (list ksha '3b4b3 kcommitted "2026-01-05_16-00-00" kauthor (person "name" "fp") kcommitter (person "name" "fp") #| more k&v |#) ; 6
   (list ksha '3b4b4 kcommitted "2026-01-08_12-23-00" kauthor (person "name" "fp") kcommitter (person "name" "fp") #| more k&v |#) ; 7
   (list ksha '3b4b5 kcommitted "2026-01-12_00-44-55" kauthor (person "name" "fp") kcommitter (person "name" "fp") #| more k&v |#) ; 8
   ))

;; (append base (list ...)) creates a fresh list by copying base. Use cons* for
;; better performance

(define fork1
  (list
   (cons* kattested "2026-01-01_13-00-20" (list-ref new-commits 0))
   (cons* kattested "2026-01-01_13-00-20" (list-ref new-commits 1))
   (cons* kattested "2026-01-01_13-00-20" (list-ref new-commits 2))
   (cons* kattested "2026-01-02_19-00-23" (list-ref new-commits 3))
   (cons* kattested "2026-01-02_19-00-23" (list-ref new-commits 4))
   (cons* kattested "2026-01-06_12-38-01" (list-ref new-commits 5))
   (cons* kattested "2026-01-07_12-40-24" (list-ref new-commits 6))
   ))

(define fork2
  (list
   (cons* kattested "2026-01-01_23-40-00" (list-ref new-commits 0))
   (cons* kattested "2026-01-01_23-40-00" (list-ref new-commits 1))
   (cons* kattested "2026-01-01_23-40-00" (list-ref new-commits 2))
   (cons* kattested "2026-01-04_09-11-00" (list-ref new-commits 3))
   (cons* kattested "2026-01-04_09-11-00" (list-ref new-commits 4))
   (cons* kattested "2026-01-10_09-15-33" (list-ref new-commits 5))
   (cons* kattested "2026-01-10_09-15-33" (list-ref new-commits 6))
   (cons* kattested "2026-01-10_09-15-33" (list-ref new-commits 7))
   ))

(define fork3
  (list
   (cons* kattested "2026-01-10_09-58-07" (list-ref new-commits 0))
   (cons* kattested "2026-01-10_09-58-07" (list-ref new-commits 1))
   (cons* kattested "2026-01-10_09-58-07" (list-ref new-commits 2))
   (cons* kattested "2026-01-10_09-58-07" (list-ref new-commits 3))
   (cons* kattested "2026-01-10_09-58-07" (list-ref new-commits 4))
   (cons* kattested "2026-01-10_09-58-07" (list-ref new-commits 5))
   (cons* kattested "2026-01-10_09-58-07" (list-ref new-commits 6))
   ))
