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
  ;; #:use-module (ice-9 pretty-print)
  ;; scandir nftw
  #:use-module (ice-9 ftw)
  ;; string-match
  #:use-module (ice-9 regex)
  )

(define m (module-name-for-logging))
(evaluating-module)

(define notes-dir "org-roam")

(define (list-all-files path)
  "(list-all-files notes-dir)"
  (let ((files '()))
    (define (file-collector filename statinfo flag base level)
      (when (equal? 'regular flag) ; it's a regular file
        (set! files (cons filename files)))
      #t)         ; continue traversing
    (nftw path file-collector)
    files))

(define (expand-pattern relative-dir pattern)
  "Examples:
(expand-pattern relative-dir \".*\")  ;; crep
(expand-pattern relative-dir \"cli/git\")
(expand-pattern relative-dir \"cli/\")
(expand-pattern relative-dir \"cvs\")
"
  (let* [(absolute-dir (str "/home/bost/" relative-dir))]
    (remove
     (lambda (file) (ends-with? (dirname file) "compiled"))
     (if (string= ".*" pattern)
         (list-all-files absolute-dir)
         (let* [(re (let* [(b (basename pattern))]
                      (str (if (string-suffix? "/" b) "" ".*") b ".*")))
                (dir (str absolute-dir "/"
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
             (lambda (p) (or p (list))) ;; the dir may not exist
             (partial scandir dir))
            (lambda (s) (string-match (basename re) s))))))))

(define launcher-spacemacs (str "emacs-launcher-" spacemacs))
(define launcher-spguimacs (str "emacs-launcher-" spguimacs))
(define launcher-crafted   (str "emacs-launcher-" crafted))
(define launcher-lst       (list launcher-spacemacs
                                 launcher-spguimacs
                                 launcher-crafted))

(define editable-spacemacs (str "emacs-editable-" spacemacs))
(define editable-spguimacs (str "emacs-editable-" spguimacs))
(define editable-crafted   (str "emacs-editable-" crafted))
(define editable-lst       (list editable-spacemacs
                                 editable-spguimacs
                                 editable-crafted))

(define pkill-spacemacs (str "emacs-pkill-" spacemacs))
(define pkill-spguimacs (str "emacs-pkill-" spguimacs))
(define pkill-crafted   (str "emacs-pkill-" crafted))
(define pkill-lst       (list pkill-spacemacs
                              pkill-spguimacs
                              pkill-crafted))

(define (full-filepaths patterns)
  "Returns a string containing paths. E.g.:
(full-filepaths (list \"ai\")) =>
(list \"/home/bost/org-roam/ai.scrbl\"
      \"/home/bost/org-roam/mainframe_and_host.scrbl\"
      \"/home/bost/org-roam/main.rkt\")
"
  ((comp
    ;; This is not needed. The string will be surrounded by single quotes.
    ;; (partial format #f "\"~a\"")
    ;; string-join
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
  ;; (format #t "~a scheme-file-name : ~s; program-name : ~s\n" m scheme-file-name program-name)
  `(,(str scm-bin-dirname "/" program-name)
    ,(program-file
      (cond
       [(equal? scheme-file-name "chmod")
        (str "chmod-plus-" chmod-params)]
       [(equal? scheme-file-name "search-notes")
        (str "search-notes-" program-name)]
       [(member scheme-file-name (append launcher-lst editable-lst pkill-lst))
        scheme-file-name]
       [#t
        desc])
      ;; TODO clarify if source-module-closure is needed only for imports of
      ;; guix modules?
      (let* ((symb-string (or scheme-file-name program-name))
             (symb (or module-name
                       (string->symbol symb-string)))
             (main-call
              (remove unspecified?
                      `(main ,(cond
                               [(equal? scheme-file-name "chmod")
                                `(let [(cmd-line (command-line))]
                                   (append (list (car cmd-line)
                                                 ,chmod-params)
                                           (cdr cmd-line)))]

                               [(equal? scheme-file-name "search-notes")
                                `(append (command-line)
                                         (list ,@(append other-files
                                                         (full-filepaths files))))]

                               [#t `(command-line)])))))
        (with-imported-modules
            ((comp
              (partial remove unspecified?)
              (lambda (lst)
                (if (equal? scheme-file-name "search-notes")
                    (append lst `(
                                  (guix profiling)
                                  (guix memoization)
                                  (guix colors)
                                  ;; (ice-9 getopt-long)
                                  ))
                    lst)))
             `((guix monads)
               (utils)
               (settings)
               ;; following three modules don't need to be everywhere
               (scm-bin gre)
               (scm-bin gps)

               ,(begin
                  (cond
                   [(member scheme-file-name
                            (append launcher-lst editable-lst pkill-lst))
                    `(emacs-common)]))

               ;; module-search-notes
               ;; 'ls' is needed only for 'lf.scm'
               ,(cond
                 [(equal? symb-string "lf")
                  `(scm-bin ls)]

                 [(equal? scheme-file-name "chmod")
                  `(scm-bin ,symb)]

                 [(equal? scheme-file-name "search-notes")
                  `(scm-bin ,symb)]

                 [#t
                  `(scm-bin ,symb)])))
          #~(begin
              (use-modules (scm-bin #$symb))
              #$main-call))))))
(testsymb 'service-file)

(define search-notes-service-files
  (list
   (let [(other-files
          (flatten
           (append
            (map (lambda (pattern)
                   (expand-pattern "dec/corona_cases" pattern))
                 (list
                  "end"
                  "clj"
                  "src/corona/"
                  "src/corona/api/"
                  "src/corona/models/"
                  "src/corona/msg/graph/"
                  "src/corona/msg/text/"
                  "src/corona/web/"
                  "test/corona/"))
            (map (lambda (pattern)
                   (expand-pattern "dec/fdk" pattern))
                 (list
                  "end"
                  "clj"
                  "data/src/fdk/datasrc/"
                  "data/src/fdk/"
                  "data/test/fdk/"
                  "env/dev/clj/fdk/cmap/"
                  "env/dev/clj/"
                  "env/prod/clj/"
                  "env/prod/clj/fdk/cmap/"
                  "src/clj/fdk/cmap/"
                  "src/clj/fdk/cmap/web/controllers/"
                  "src/clj/fdk/cmap/web/"
                  "src/clj/fdk/cmap/web/middleware/"
                  "src/clj/fdk/cmap/web/pages/"
                  "src/clj/fdk/cmap/web/routes/"
                  "src/clj/fdk/data/"
                  "test/clj/fdk/cmap/"
                  "src/cljs/fdk/cmap/"))))
          )]
     (service-file #:program-name "crc"
                   #:files (list "lisp/clojure")
                   #:other-files other-files
                   #:scheme-file-name "search-notes"))
   (let [(other-files
         ((comp
           (partial apply append)
           (partial map (lambda (params) (apply expand-pattern params))))
          (list
           (list ".emacs.d.distros/spguimacs" "core/el")
           (list "dev/kill-buffers" "el")
           (list "dev/dotfiles" ".sp.*macs")
           (list "dev/jump-last" "el")
           (list "dev/tweaks" "el")
           (list "dev/farmhouse-light-mod-theme" "el"))))]
     (service-file #:program-name "cre"
                   #:files (list "editors/")
                   #:other-files other-files
                   #:scheme-file-name "search-notes"))
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
                  (expand-pattern "dev/guix" "scm")
                  )
                 #:scheme-file-name "search-notes")
;;; TODO crgi should also search in the output of `git config --get' etc.
   (service-file #:program-name "crgi"
                 #:files (list "cli/git")
                 #:other-files
                 (append
                  (expand-pattern "dev/dotfiles" ".gitconfig")
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
                  (expand-pattern "dev/dotfiles" ".bash")
                  )
                 #:scheme-file-name "search-notes")
   (service-file #:program-name "crli"
                 #:files (list "cli/listing")
                 #:scheme-file-name "search-notes")
   (service-file #:program-name "crr"
                 #:files (list "lisp/racket")
                 #:other-files
                 (append
                  (expand-pattern "der/search-notes" "rkt")
                  )
                 #:scheme-file-name "search-notes")
;;; TODO create crct - search in category-theory notes
;;; TODO crs should be like crl
   (service-file #:program-name "crs"
                 #:files (list "cli/shells")
                 ;; #:other-files
                 ;; (append
                 ;;  (expand-pattern "dev/dotfiles" ".bash")
                 ;;  )
                 #:scheme-file-name "search-notes")
   (service-file #:program-name "cru"
                 #:files (list "utf8")
                 #:scheme-file-name "search-notes")))

(define-public (scheme-files-service)
  ((comp
    (partial simple-service 'scheme-files-service home-files-service-type)
    (partial
     append
     (cond
      [(or (is-system-ecke) (is-system-edge))
       (append
        search-notes-service-files
        (list
         (service-file #:program-name "s"  #:scheme-file-name launcher-spacemacs)
         (service-file #:program-name "es" #:scheme-file-name editable-spacemacs)
         (service-file #:program-name "ks" #:scheme-file-name pkill-spacemacs)

         (service-file #:program-name "g"  #:scheme-file-name launcher-spguimacs)
         (service-file #:program-name "eg" #:scheme-file-name editable-spguimacs)
         (service-file #:program-name "kg" #:scheme-file-name pkill-spguimacs)

         (service-file #:program-name "r"  #:scheme-file-name launcher-crafted)
         (service-file #:program-name "er" #:scheme-file-name editable-crafted)
         (service-file #:program-name "kr" #:scheme-file-name pkill-crafted)
        ))]
      [#t
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
    (service-file #:program-name "gpg-pinentry-setup" #:desc "gpg-pinentry-setup")
    (service-file #:program-name "l"   #:desc "list-dir-contents"
                  #:scheme-file-name "ls")
    (service-file #:program-name "lf"
                  #:desc "list-directory-contents-with-full-paths")
    (service-file #:program-name "lt"  #:desc "list-dir-sorted-by-time-descending"
                  #:scheme-file-name "lt")
    (service-file #:program-name "lT"  #:desc "list-dir-sorted-by-time-ascending"
                  #:scheme-file-name "lT")
    (service-file #:program-name "qemu-vm" #:desc "qemu-vm")
    (service-file #:program-name "susp" #:desc "suspend-to-ram")
    )))
(testsymb 'scheme-files-service)

(module-evaluated)
