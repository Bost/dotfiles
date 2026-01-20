(define-module (dotf attest data)
  #:use-module (srfi srfi-1)   ; list-processing procedures
  #:use-module (srfi srfi-19)  ; current-date
  #:use-module (srfi srfi-88)  ; provides keyword objects
  #:use-module (guix channels) ; %default-guix-channel %default-channels
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

(define kurl :u)
(define ktype :t)
(define kplatform :p)


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

;;; ----------------------------------------------------------------------------
(define (parse-ls-remote-line line)
  "Parse: 'commit-hash\trefs/tags/tag-name'"
  (match (string-split line #\tab)
    ((hash ref)
     (let ((tag-name (string-drop ref (string-length "refs/tags/"))))
       (and (string-prefix? "running-" tag-name)
            (make-tag-info tag-name hash))))
    ;; (_ #f)
    ))

(define (fetch-git-ls-remote-tags url)
  "Use git ls-remote to get tags without clone.

git ls-remote --tags <url>
git ls-remote --heads <url>

git ls-remote --heads https://git.sr.ht/~abcdw/rde
git ls-remote --heads https://github.com/Bost/guix.git
git ls-remote --heads https://gitlab.com/rostislav.svoboda/guix.git

#!/bin/bash

REPO_URL=\"https://github.com/Bost/guix.git\"
git ls-remote --heads $REPO_URL | awk '{print $2}' | grep --extended-regexp '^refs/tags/' | cut --delimiter='/' -f3

"
  (let* ((output (with-output-to-string
                  (lambda ()
                    (system* "git" "ls-remote" "--tags" url))))
         (lines (string-split output #\newline)))
    (filter-map parse-ls-remote-line lines)))

(define (fetch-github-tags url)
  "Use GitHub API for efficiency.

curl --silent https://api.github.com/repos/<owner>/<repo>/tags

curl --silent https://api.github.com/repos/Bost/guix/tags
"
  (let* ((repo-path (extract-github-repo url))
         (api-url (string-append
                   "https://api.github.com/repos/"
                   repo-path "/tags"))
         (json (http-get-json api-url)))
    (filter-map
     (lambda (tag-obj)
       (let ((name (assoc-ref tag-obj "name")))
         (and (string-prefix? "running-" name)
              (make-tag-info
               name
               (assoc-ref (assoc-ref tag-obj "commit") "sha")))))
     json)))

(define (fetch-gitlab-tags url)
  "Use GitLab API.

curl --silent https://gitlab.com/api/v4/projects/<project-id>/
(project-id is under the three vertical dots ⋮ on the upper right side)

curl --silent https://gitlab.com/api/v4/projects/67989111/
"
  (let* ((project-id (extract-gitlab-project-id url))
         (api-url (string-append
                   "https://gitlab.com/api/v4/projects/"
                   (uri-encode project-id)
                   "/repository/tags"))
         (json (http-get-json api-url)))
    (filter-map
     (lambda (tag-obj)
       (let ((name (assoc-ref tag-obj "name")))
         (and (string-prefix? "running-" name)
              (make-tag-info
               name
               (assoc-ref tag-obj "commit" "id")))))
     json)))

(define (fetch-remote-tags url)
  "Fetch tags from remote git repo without cloning."
  (cond
   ((string-contains url "github.com")   (fetch-github-tags url))
   ((string-contains url "gitlab.com")   (fetch-gitlab-tags url))
   ;; Codeberg (Gitea API)
   ((string-contains url "codeberg.org") (fetch-gitea-tags url))
   ;; Generic git (any hosting)
   (else                                 (fetch-git-ls-remote-tags url))))

(define %default-attestation-sources
  (list
   ;; Guix-based distributions
   (list kname "RDE (Andrew Tropin)" kurl "https://git.sr.ht/~abcdw/rde" ktype 'channel)
   (list kname "Guix-HPC"            kurl "https://gitlab.inria.fr/guix-hpc/guix-hpc" ktype 'channel)
   (list kname "Efraim's channel"    kurl "https://git.sr.ht/~efraim/my-guix" ktype 'channel)
   ))

(define last-week-non-codeberg-forks
  '(
    https://github.com/Millak/guix                  ; Efraim Flashner
    https://gitlab.com/debdistutils/guix/mirror     ; probably automatic
    https://https.git.savannah.gnu.org/git/guix.git ; automatic
    ))

;;; forks with an update done 'last week' of later; checked on:
;;; 2025-12-26 ; roughly; it was checked by the end of December 2025
;;; 2026-01-19 ; i.e. after ~3 weeks
;;; 25 out of ~348 forks only on Codeberg
(define last-week-codeberg-forks
  '(
    https://codeberg.org/lechner/guix-mirror
    https://codeberg.org/group/guix
    https://codeberg.org/Yelninei/guix
    https://codeberg.org/cmargiotta/guix
    https://codeberg.org/museoa/guix
    https://codeberg.org/hebasto/guix
    https://codeberg.org/TohsakaTypeclass/guix
    https://codeberg.org/sikmir/guix
    https://codeberg.org/gabber/guix
    https://codeberg.org/jnms/guix
    https://codeberg.org/fanquake/guix
    https://codeberg.org/r0man/guix
    https://codeberg.org/FuncProgLinux/guix
    https://codeberg.org/Doom4535/guix
    https://codeberg.org/ajad/guix
    https://codeberg.org/aquohn/guix
    https://codeberg.org/mst/guix
    https://codeberg.org/rodion-goritskov/guix
    https://codeberg.org/kinote/guix
    https://codeberg.org/ravenjoad/guix
    https://codeberg.org/PatrickNorton/guix
    https://codeberg.org/foster-hangdaan/guix
    https://codeberg.org/frankie/guix
    https://codeberg.org/yqshao/guix
    https://codeberg.org/retropikzel/guix
    ))

;; See /home/bost/dev/whereiseveryone-toys/channels.scm
;; Check the project stars


;;; forks with an update done 'last week' of later; checked on:
;;; 2025-12-26 ; roughly; it was checked by the end of December 2025
;;; 2026-01-19 ; i.e. after ~3 weeks
;;; 25 out of ~348 forks only on Codeberg
(define last-week-codeberg-forks
  '(
    https://codeberg.org/lechner/guix-mirror
    https://codeberg.org/group/guix
    https://codeberg.org/Yelninei/guix
    https://codeberg.org/cmargiotta/guix
    https://codeberg.org/museoa/guix
    https://codeberg.org/hebasto/guix
    https://codeberg.org/TohsakaTypeclass/guix
    https://codeberg.org/sikmir/guix
    https://codeberg.org/gabber/guix
    https://codeberg.org/jnms/guix
    https://codeberg.org/fanquake/guix
    https://codeberg.org/r0man/guix
    https://codeberg.org/FuncProgLinux/guix
    https://codeberg.org/Doom4535/guix
    https://codeberg.org/ajad/guix
    https://codeberg.org/aquohn/guix
    https://codeberg.org/mst/guix
    https://codeberg.org/rodion-goritskov/guix
    https://codeberg.org/kinote/guix
    https://codeberg.org/ravenjoad/guix
    https://codeberg.org/PatrickNorton/guix
    https://codeberg.org/foster-hangdaan/guix
    https://codeberg.org/frankie/guix
    https://codeberg.org/yqshao/guix
    https://codeberg.org/retropikzel/guix
    ))
