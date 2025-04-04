(define-module (srvc scheme-files)
  ;; #:use-module (cfg packages-new)
  #:use-module (utils)
  #:use-module (tests)
  #:use-module (settings)
  #:use-module (memo)
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
(testsymb 'list-all-files)

(define (expand-pattern relative-dir pattern)
  "Examples:
(expand-pattern relative-dir \".*\")  ;; crep
(expand-pattern relative-dir \"cli/git\")
(expand-pattern relative-dir \"cli/\")
(expand-pattern relative-dir \"cvs\")
"
  (let* [(m (format #f "~a [expand-pattern]" m))]
    (let* [(absolute-dir (str home "/" relative-dir))]
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
              (lambda (s) (string-match (basename re) s)))))))))
(testsymb 'expand-pattern)

;; ;; (define-syntax define-emacs-utils
;; ;;   (syntax-rules ()
;; ;;     ((_ name ...)
;; ;;      ((comp
;; ;;        #;(lambda (p) (format #t "p: ~a\n" p) p))
;; ;;       (list
;; ;;        (define launcher-<util-name> (str "emacs-launcher-" util-name))
;; ;;        (define editable-<util-name> (str "emacs-editable-" util-name))
;; ;;        (define pkill-<util-name> (str "emacs-pkill-" util-name))
;; ;;        )))))

;; (define* (create type name)
;;   "(create 'launcher 'spacemacs)"
;;   ;; (format #t "util-type: ~a; util-name: ~a\n" type name)
;;   ;; (format #t "(test-type type): ~a\n" (test-type type))
;;   ;; (format #t "util-type: ~a\n" type)
;;   ;; (format #t "(test-type name): ~a\n" (test-type name))
;;   ;; (format #t "util-name: ~a\n" name)
;;   (let* [(stype (symbol->string type))
;;          (sname (symbol->string name))
;;          (name (string->symbol (format #f "~a-~a" stype sname)))
;;          (body (format #f "emacs-~a-~a" stype sname))]
;;     (let [(result `(define ,name ,body))]
;;       ;; (format #t "Creating ~a\n" result)
;;       result)))

;; ;; " (define-util-names 'spacemacs) "
;; (define (define-utils)
;;   (begin
;;     (let* [
;;            ;; (util-types (list 'ylauncher 'editable 'pkill))
;;            ;; (util-names (list 'spacemacs))

;;            (util-types (list 't1 't2))
;;            (util-names (list 'n1 'n2))
;;            (params (cartesian util-types util-names))
;;            ]
;;       ;; params
;;       (map (partial apply create) params))
;;     ))

;; ;; (list
;; ;;  '(define t1-n1 "emacs-t1-n1")
;; ;;  '(define t1-n2 "emacs-t1-n2")
;; ;;  '(define t2-n1 "emacs-t2-n1")
;; ;;  '(define t2-n2 "emacs-t2-n2"))

;; (define-syntax define-emacs-utils
;;   "(define-emacs-utils spacemacs)"
;;   (lambda (stx)
;;     (syntax-case stx ()
;;       [(_ util)
;;        (let* ((symbol (syntax->datum #'util))
;;               (util-str (symbol->string symbol))
;;               (launcher-id (datum->syntax #'util (symbol-append 'launcher- symbol)))
;;               (editable-id (datum->syntax #'util (symbol-append 'editable- symbol)))
;;               (pkill-id    (datum->syntax #'util (symbol-append 'pkill-    symbol))))
;;          #`(begin
;;              (define #,launcher-id (string-append "emacs-launcher-" #,util-str))
;;              (define #,editable-id (string-append "emacs-editable-" #,util-str))
;;              (define #,pkill-id    (string-append "emacs-pkill-"    #,util-str))))])))

(define launcher-develop   (str "emacs-launcher-" develop))
(define launcher-spguimacs (str "emacs-launcher-" spguimacs))
(define launcher-crafted   (str "emacs-launcher-" crafted))
(define launcher-lst       (list
                            launcher-develop
                            launcher-spguimacs
                            launcher-crafted
                            ))

(define editable-profiles "emacs-editable-profiles") ;; ~/.emacs-profiles.el

(define editable-develop   (str "emacs-editable-" develop))
(define editable-spguimacs (str "emacs-editable-" spguimacs))
(define editable-crafted   (str "emacs-editable-" crafted))
(define editable-lst       (list
                            editable-profiles
                            editable-develop
                            editable-spguimacs
                            editable-crafted
                            ))

(define pkill-develop   (str "emacs-pkill-" develop))
(define pkill-spguimacs (str "emacs-pkill-" spguimacs))
(define pkill-crafted   (str "emacs-pkill-" crafted))
(define pkill-lst       (list
                         pkill-develop
                         pkill-spguimacs
                         pkill-crafted
                         ))
(testsymb 'pkill-lst)

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
(testsymb 'full-filepaths)

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
  (let* [(m (format #f "~a [search-notes-service-files]" m))]
    ;; (lambda (v) (format #t "~a starting...\n" m) v)
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
                            ;; (lambda (v) (format #t "~a 3\n" m) v)
                            (partial apply append)
                            ;; (lambda (v) (format #t "~a 2\n" m) v)
                            (partial map (partial apply expand-pattern))
                            ;; (lambda (v) (format #t "~a 1\n" m) v)
                            )
                           (list
                            ;; (get-src spguimacs) points to a spacemacs-distros
                            (list (substring (get-src spguimacs)
                                             (string-length (str home "/")))
                                  "core/el")
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
            #:scheme-file "search-notes")))))
(testsymb 'search-notes-service-files)

(define-public (scheme-files-service)
  (let* [(m (format #f "~a [scheme-files-service]" m))]
    ;; (lambda (v) (format #t "~a 0 starting...\n" m) v)
    ((comp
      ;; (lambda (v) (format #t "~a done\n" m) v)
      (partial simple-service 'scheme-files-service home-files-service-type)
      (partial
       append
       (if (or (is-system-ecke) (is-system-edge))
           ((comp
             (partial append search-notes-service-files)
             (partial map (partial apply service-file))
             )
            (list
             (list #:program-name "ep" #:scheme-file editable-profiles)

             (list #:program-name  "d" #:scheme-file launcher-develop)
             (list #:program-name "ed" #:scheme-file editable-develop)
             (list #:program-name "kd" #:scheme-file pkill-develop)

             (list #:program-name  "g" #:scheme-file launcher-spguimacs)
             (list #:program-name "eg" #:scheme-file editable-spguimacs)
             (list #:program-name "kg" #:scheme-file pkill-spguimacs)

             (list #:program-name  "r" #:scheme-file launcher-crafted)
             (list #:program-name "er" #:scheme-file editable-crafted)
             (list #:program-name "kr" #:scheme-file pkill-crafted)
             ))
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
      (list #:program-name "susp" #:desc "suspend-to-ram")))))
(testsymb 'scheme-files-service)

(module-evaluated)
