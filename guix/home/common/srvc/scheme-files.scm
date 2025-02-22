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
                       program-name desc scheme-file module-name
                       chmod-params files
                       (other-files (list)))
  "The priority is 1. module-name, 2. scheme-file, 3. program-name

TODO The `search-notes' program should read a `search-space-file' containing
a list of files to search through.

Example:
    chmod --recursive u=rwx,g=rwx,o=rwx /path/to/dir
"
  `(,(str scm-bin-dirname "/" program-name)
    ,(program-file
      (cond
       [(equal? scheme-file "chmod")
        (str "chmod-plus-" chmod-params)]
       [(equal? scheme-file "search-notes")
        (str "search-notes-" program-name)]
       [(member scheme-file (append launcher-lst editable-lst pkill-lst))
        scheme-file]
       [#t
        desc])
      ;; TODO clarify if source-module-closure is needed only for imports of
      ;; guix modules?
      (let* ((symb-string (or scheme-file program-name))
             (symb (or module-name
                       (string->symbol symb-string)))
             (main-call
              (remove unspecified?
                      `(main ,(cond
                               [(equal? scheme-file "chmod")
                                `(let [(cmd-line (command-line))]
                                   (append (list (car cmd-line)
                                                 ,chmod-params)
                                           (cdr cmd-line)))]

                               [(equal? scheme-file "search-notes")
                                `(append
                                  (command-line)
                                  (list ,@(append other-files
                                                  (full-filepaths files))))]

                               [#t `(command-line)])))))
        (with-imported-modules
            ((comp
              (partial remove unspecified?)
              (lambda (lst)
                (if (equal? scheme-file "search-notes")
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
                   [(member scheme-file
                            (append launcher-lst editable-lst pkill-lst))
                    `(emacs-common)]))

               ;; module-search-notes
               ;; 'ls' is needed only for 'lf.scm'
               ,(cond
                 [(equal? symb-string "lf")           `(scm-bin ls)]
                 [(equal? scheme-file "chmod")        `(scm-bin ,symb)]
                 [(equal? scheme-file "search-notes") `(scm-bin ,symb)]
                 [#t                                  `(scm-bin ,symb)])))
          #~(begin
              (use-modules (scm-bin #$symb))
              #$main-call))))))
(testsymb 'service-file)

(define search-notes-service-files
  (map
   (partial apply service-file)
   (list
    (list #:program-name "crc"
          #:files (list "lisp/clojure")
          #:other-files (flatten
                         (append
                          (map (partial expand-pattern "dec/corona_cases")
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
                          (map (partial expand-pattern "dec/fdk")
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
          #:scheme-file "search-notes")

    (list #:program-name "cre"
          #:files (list "editors/")
          #:other-files ((comp
                          (partial apply append)
                          (partial map (partial apply expand-pattern)))
                         (list
                          (list ".emacs.d.distros/spguimacs" "core/el")
                          (list "dev/kill-buffers" "el")
                          (list "dev/dotfiles" ".sp.*macs")
                          (list "dev/jump-last" "el")
                          (list "dev/tweaks" "el")
                          (list "dev/farmhouse-light-mod-theme" "el")))
          #:scheme-file "search-notes")
    (list #:program-name "crep"
          #:files (list ".*") ;; TODO exclude /home/bost/org-roam/notes.scrbl
          #:scheme-file "search-notes")
    (list #:program-name "cra"
          #:files (list "ai")
          #:scheme-file "search-notes")
    (list #:program-name "crf"
          #:files (list "cli/find_and_grep")
          #:scheme-file "search-notes")
;;; TODO crg should also search in the $dotf/guix/
;;; XXX Bug: 'Replicating' is on the line 137, not on 118
;;; $ cd ~ && crg Replicating
;;; /home/bost/org-roam/guix-guile-nix/nix.scrbl
;;; 115	@block{@block-name{NixOS Flakes}
;;; 116    - like guix time-machine with channels
;;; 117    https://logs.guix.gnu.org/guix/2023-01-31.log#094922
;;; 118    See 7.3 Replicating Guix in manual
;;; 119    https://guix.gnu.org/manual/devel/en/html_node/Replicating-Guix.html
;;; 120	}
    (list #:program-name "crg" #:files (list "guix-guile-nix/")
          #:other-files (append (expand-pattern "dev/guix" "scm"))
          #:scheme-file "search-notes")
;;; TODO crgi should also search in the output of `git config --get' etc.
    (list #:program-name "crgi" #:files (list "cli/git")
          #:other-files (append (expand-pattern "dev/dotfiles" ".gitconfig"))
          #:scheme-file "search-notes")
;;; TODO crl should search in the $dotf/.config/fish and other profile files
    (list #:program-name "crl" #:files (list "guix-guile-nix/" "cli/"
                                             ;; simple files
                                             "network" "cvs" "gui")
          #:other-files (append (expand-pattern "dev/dotfiles" ".bash"))
          #:scheme-file "search-notes")
    (list #:program-name "crli" #:files (list "cli/listing")
          #:scheme-file "search-notes")
    (list #:program-name "crr" #:files (list "lisp/racket")
          #:other-files (append (expand-pattern "der/search-notes" "rkt"))
          #:scheme-file "search-notes")
;;; TODO create crct - search in category-theory notes
;;; TODO crs should be like crl
    (list #:program-name "crs" #:files (list "cli/shells")
          ;; #:other-files (append (expand-pattern "dev/dotfiles" ".bash"))
          #:scheme-file "search-notes")
    (list #:program-name "cru" #:files (list "utf8")
          #:scheme-file "search-notes"))))

(define-public (scheme-files-service)
  ((comp
    (partial simple-service 'scheme-files-service home-files-service-type)
    (partial
     append
     (if (or (is-system-ecke) (is-system-edge))
         ((comp
           (partial append search-notes-service-files)
           (partial map (partial apply service-file)))
          (list
           (list #:program-name "s"  #:scheme-file launcher-spacemacs)
           (list #:program-name "es" #:scheme-file editable-spacemacs)
           (list #:program-name "ks" #:scheme-file pkill-spacemacs)

           (list #:program-name "g"  #:scheme-file launcher-spguimacs)
           (list #:program-name "eg" #:scheme-file editable-spguimacs)
           (list #:program-name "kg" #:scheme-file pkill-spguimacs)

           (list #:program-name "r"  #:scheme-file launcher-crafted)
           (list #:program-name "er" #:scheme-file editable-crafted)
           (list #:program-name "kr" #:scheme-file pkill-crafted)))
         (list)))
    (partial map (partial apply service-file)))
   (list
    ;; pwr and prw do the same
    (list #:program-name "pwr" #:chmod-params "rw" #:scheme-file "chmod")
    (list #:program-name "prw" #:chmod-params "rw" #:scheme-file "chmod")
    (list #:program-name "px"  #:chmod-params "x"  #:scheme-file "chmod")
    (list #:program-name "ext"  #:desc "extract-uncompress"
          #:scheme-file "extract")
    (list #:program-name "c"    #:desc "batcat" #:scheme-file "bat")
    (list #:program-name "f"    #:desc "find-alternative")
    (list #:program-name "gcl"  #:desc "git-clone")
    (list #:program-name "gre"  #:desc "git-remote")
    (list #:program-name "grev" #:desc "git-remote--verbose")
    (list #:program-name "gfe"  #:desc "git-fetch")
    (list #:program-name "gco"  #:desc "git-checkout")
    (list #:program-name "gcod" #:desc "git-checkout-prev-branch")
    (list #:program-name "gcom" #:desc "git-checkout-master")
    (list #:program-name "gg"   #:desc "git-gui")
    (list #:program-name "gps"  #:desc "git-push")
    (list #:program-name "gpsf" #:desc "git-push--force")
    (list #:program-name "gk"   #:desc "git-repo-browser")
    (list #:program-name "gpl"  #:desc "git-pull--rebase")
    (list #:program-name "gs"   #:desc "git-status")
    (list #:program-name "gtg"  #:desc "git-tag")
    (list #:program-name "gpg-pinentry-setup" #:desc "gpg-pinentry-setup")
    (list #:program-name "l"    #:desc "list-dir-contents" #:scheme-file "ls")
    (list #:program-name "lf"   #:desc "list-dir-contents-with-full-paths")
    (list #:program-name "lt"   #:desc "list-dir-sorted-by-time-descending"
          #:scheme-file "lt")
    (list #:program-name "lT"   #:desc "list-dir-sorted-by-time-ascending"
          #:scheme-file "lT")
    (list #:program-name "qemu-vm" #:desc "qemu-vm")
    (list #:program-name "susp" #:desc "suspend-to-ram"))))
(testsymb 'scheme-files-service)

(module-evaluated)
