(define-module (srvc scheme-files)
  ;; #:use-module (cfg packages-new)
  #:use-module (memo)
  #:use-module (utils)
  #:use-module (fs-utils)
  ;; See service-file -> with-imported-modules
  #:use-module (scm-bin gcl)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  ;; program-file local-file
  #:use-module (guix gexp)
  #:use-module (gnu home services shells)
  ;; simple-service
  #:use-module (gnu home services)
  ;; take remove delete-duplicates append-map etc.
  #:use-module (srfi srfi-1)
  ;; pretty-print
  ;; #:use-module (ice-9 pretty-print)
  ;; scandir nftw
  #:use-module (ice-9 ftw)
 ;; string-match
  #:use-module (ice-9 regex)
)

(evaluating-module)

(define notes-dir (user-home "/org-roam"))

(define (list-all-files path)
  "(list-all-files notes-dir)"
  (let ((files '()))
    (define (file-collector filename statinfo flag base level)
      (when (equal? 'regular flag) ; it's a regular file
        (set! files (cons filename files)))
      #t)         ; continue traversing
    (nftw path file-collector)
    files))

(define (expand-pattern notes-dir pattern)
  "Examples:
(expand-pattern notes-dir \".*\")  ;; crep
(expand-pattern notes-dir \"cli/git\")
(expand-pattern notes-dir \"cli/\")
(expand-pattern notes-dir \"cvs\")
"
  (if (string= ".*" pattern)
      (list-all-files notes-dir)
      (let* [(re (let* [(b (basename pattern))]
                   (str (if (string-suffix? "/" b) "" ".*") b ".*")))
             (dir (str notes-dir "/"
                       (let* [(d (dirname pattern))]
                         (if (string= "." d)   "" (str d "/")))
                       (let* [(dre (dirname re))]
                         (if (string= "." dre) "" (str dre "/")))))
             (rem-fn (if (string= pattern ".*")
                         (lambda _ #f)
                         file-is-directory?))]
        ((comp
          (partial remove rem-fn)
          (partial map (partial str dir))
          (partial remove (lambda (f) (member f (list "." ".."))))
          (lambda (p) (or p (list))) ;; the notes-dir may not exist
          (partial scandir dir))
         (lambda (s) (string-match (basename re) s))))))

(define (full-filepaths patterns)
  "Returns a string containing paths. E.g.:
(full-filepaths (list \"ai\")) =>
\"/home/bost/org-roam/ai.scrbl /home/bost/org-roam/mainframe_and_host.scrbl /home/bost/org-roam/main.rkt\"
"
  ((comp
    ;; This is not needed. The string will be surrounded by single quotes.
    ;; (partial format #f "\"~a\"")
    string-join
    flatten
    (partial map (partial expand-pattern notes-dir)))
   patterns))

(define* (service-file #:key
                       program-name desc scheme-file-name module-name
                       chmod-params files
                       (other-files (list)))
  "The priority is 1. module-name, 2. scheme-file-name, 3. program-name

TODO The `search-notes' program should read a `search-space-file' containing
a list of files to search through.

Example:
    chmod --recursive u=rwx,g=rwx,o=rwx /path/to/dir
"
  `(,(str scm-bin-dirname "/" program-name)
    ,(program-file
      (cond
       ((equal? scheme-file-name "chmod")
        (str "chmod-plus-" chmod-params))
       ((equal? scheme-file-name "search-notes")
        (str "search-notes-" program-name))
       (#t
        desc))
      ;; TODO clarify if source-module-closure is needed only for imports of
      ;; guix modules?
      (let* ((symb-string (or scheme-file-name program-name))
             (symb (or module-name
                       (string->symbol symb-string)))
             (main-call
              (remove unspecified?
                      `(main ,(cond
                               ((equal? scheme-file-name "chmod")
                                chmod-params)
                               ((equal? scheme-file-name "search-notes")
                                #;
                                (begin
                                  (format #t "(list? ~a)\n" (list? other-files))
                                  (full-filepaths files))
                                (string-join
                                 (append
                                  other-files
                                  (list
                                   (full-filepaths files))))))
                             (command-line)))))
        (with-imported-modules
            (remove
             unspecified?
             `((utils)
               (settings)
               (scm-bin gre)
               (scm-bin gps)

               ;; ,(cond
               ;;   ((or
               ;;     (equal? scheme-file-name "emacs-launcher")
               ;;     (equal? scheme-file-name "spguimacs-launcher"))
               ;;    `(bost utils)))

               ;; module-search-notes
               ;; 'ls' is needed only for 'lf.scm'
               ,(cond
                 ((equal? symb-string "lf")
                  `(scm-bin ls))

                 ((equal? scheme-file-name "chmod")
                  `(scm-bin ,symb))

                 ((equal? scheme-file-name "search-notes")
                  `(scm-bin ,symb))

                 (#t
                  `(scm-bin ,symb)))))
          #~(begin
              (use-modules (scm-bin #$symb))
              #$main-call))))))
(testsymb 'service-file)

(define corona-dir "/home/bost/dec/corona_cases")
(define fdk-dir "/home/bost/dec/corona_cases")

;; TODO create an alias for searching in /home/bost/dec/cheatsheet/langs/expressions.edn

(define search-notes-service-files
  (list
;;; TODO crc should search in the fdk-directories
   (service-file #:program-name "crc"
                 #:files (list "lisp/clojure")
                 #:other-files
                 (append
                  (expand-pattern corona-dir "clj")
                  (expand-pattern corona-dir "src/corona/")
                  (expand-pattern corona-dir "src/corona/api/")
                  (expand-pattern corona-dir "src/corona/models/")
                  (expand-pattern corona-dir "src/corona/msg/graph/")
                  (expand-pattern corona-dir "src/corona/msg/text/")
                  (expand-pattern corona-dir "src/corona/web/")
                  (expand-pattern corona-dir "test/corona/")
                  )
                 #:scheme-file-name "search-notes")
   (service-file #:program-name "cre"
                 #:files (list "editors/")
                 #:other-files
                 (append
                  (expand-pattern "/home/bost/.emacs.d.distros/spguimacs" "core/el")
                  (expand-pattern "/home/bost/dev/kill-buffers" "el")
                  (expand-pattern "/home/bost/dev/dotfiles" ".sp.*macs")
                  (expand-pattern "/home/bost/dev/jump-last" "el")
                  (expand-pattern "/home/bost/dev/tweaks" "el")
                  (expand-pattern "/home/bost/dev/farmhouse-light-mod-theme" "el")
                  )
                 #:scheme-file-name "search-notes")
   (service-file #:program-name "crep"
                 #:files (list ".*")
                 #:scheme-file-name "search-notes")
   (service-file #:program-name "cra"
                 #:files (list "ai")
                 #:scheme-file-name "search-notes")
   (service-file #:program-name "crf"
                 #:files (list "cli/find_and_grep")
                 #:scheme-file-name "search-notes")
;;; TODO crg should also search in the $dotf/guix/
   (service-file #:program-name "crg"
                 #:files (list "guix-guile-nix/")
                 #:other-files
                 (append
                  (expand-pattern "/home/bost/dev/guix" "scm")
                  )
                 #:scheme-file-name "search-notes")
;;; TODO crgi should also search in the output of `git config --get' etc.
   (service-file #:program-name "crgi"
                 #:files (list "cli/git")
                 #:other-files
                 (append
                  (expand-pattern "/home/bost/dev/dotfiles" ".gitconfig")
                  )
                 #:scheme-file-name "search-notes")
;;; TODO crl should search in the $dotf/.config/fish and other profile files
   (service-file #:program-name "crl"
                 #:files (list
                          "guix-guile-nix/"
                          "cli/"
                          ;; simple files
                          "network" "cvs" "gui")
                 #:other-files
                 (append
                  (expand-pattern "/home/bost/dev/dotfiles" ".bash")
                  )
                 #:scheme-file-name "search-notes")
   (service-file #:program-name "crli"
                 #:files (list "cli/listing")
                 #:scheme-file-name "search-notes")
   (service-file #:program-name "crr"
                 #:files (list "lisp/racket")
                 #:other-files
                 (append
                  (expand-pattern "/home/bost/der/search-notes" "rkt")
                  )
                 #:scheme-file-name "search-notes")
;;; TODO crs should be like crl
   (service-file #:program-name "crs"
                 #:files (list "cli/shells")
                 ;; #:other-files
                 ;; (append
                 ;;  (expand-pattern "/home/bost/dev/dotfiles" ".bash")
                 ;;  )
                 #:scheme-file-name "search-notes")
   (service-file #:program-name "cru"
                 #:files (list "utf8")
                 #:scheme-file-name "search-notes")))

(define-public scheme-files-service
  ((compose
    (partial simple-service 'scheme-files-service home-files-service-type)
    (partial
     append
     (cond
      [(is-system-geek)
       (append
        search-notes-service-files
        (list
         (service-file #:program-name "e" #:desc "emacs-launcher"
                       #:scheme-file-name "emacs-launcher")
         (service-file #:program-name "s" #:desc "spguimacs-launcher"
                       #:scheme-file-name "spguimacs-launcher")))]
      [(or (is-system-ecke) (is-system-edge))
       (append
        search-notes-service-files
        (list
         (service-file #:program-name "e" #:desc "emacs-launcher"
                       #:scheme-file-name "emacs-launcher")
         (service-file #:program-name "s" #:desc "spguimacs-launcher"
                       #:scheme-file-name "spguimacs-launcher")
;;; TODO `gui' should do `cd ~/dev/guix'
;;; TODO `guixg' should do `git pull --rebase' (preferably from a local guix
;;; checkout)
         (service-file #:program-name "qemu-vm" #:desc "qemu-virt-machine")
         (service-file #:program-name "spag"
                       #:desc "spacemacs-git-fetch-rebase")))]
      [#t
       ;; empty list
       (list)])))
   (list
    ;; pwr and prw do the same
    (service-file #:program-name "pwr"
                  #:chmod-params "rw"
                  #:scheme-file-name "chmod")
    (service-file #:program-name "prw"
                  #:chmod-params "rw"
                  #:scheme-file-name "chmod")
    (service-file #:program-name "px"
                  #:chmod-params "x"
                  #:scheme-file-name "chmod")
    (service-file #:program-name "ext" #:desc "extract-uncompress"
                  #:scheme-file-name "extract")
    (service-file #:program-name "c"   #:desc "batcat"
                  #:scheme-file-name "bat")
    (service-file #:program-name "f"   #:desc "find-alternative")
    (service-file #:program-name "gcl" #:desc "git-clone")
    ;; TODO DRY gre.scm & grev.scm, gps.scm & gspf.scm
    (service-file #:program-name "gre" #:desc "git-remote")
    (service-file #:program-name "grev" #:desc "git-remote--verbose")
    (service-file #:program-name "gfe" #:desc "git-fetch")
    (service-file #:program-name "gco" #:desc "git-checkout")
    (service-file #:program-name "gcod"#:desc "git-checkout-prev-branch")
    (service-file #:program-name "gcom"#:desc "git-checkout-master")
    (service-file #:program-name "gg"  #:desc "git-gui")
    (service-file #:program-name "gps" #:desc "git-push")
    (service-file #:program-name "gpsf" #:desc "git-push--force")
    (service-file #:program-name "gk"  #:desc "git-repo-browser")
    (service-file #:program-name "gpl" #:desc "git-pull--rebase")
    (service-file #:program-name "gs"  #:desc "git-status")
    (service-file #:program-name "gtg" #:desc "git-tag")
    ;; FIXME `l' doesn't list 7Sketches.toc
    (service-file #:program-name "l"   #:desc "list-dir-contents"
                  #:scheme-file-name "ls")
    (service-file #:program-name "lf"
                  #:desc "list-directory-contents-with-full-paths")
    (service-file #:program-name "lt"  #:desc "list-dir-sorted-by-time-descending"
                  #:scheme-file-name "lt")
    (service-file #:program-name "lT"  #:desc "list-dir-sorted-by-time-ascending"
                  #:scheme-file-name "lT")
    )))
(testsymb 'scheme-files-service)

(module-evaluated)
