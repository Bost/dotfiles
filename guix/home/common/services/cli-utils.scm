(define-module (services cli-utils)
  #:use-module (dotf srfi-1-smart)
  #:use-module (dotf utils)
  #:use-module (dotf tests)
  #:use-module (dotf settings)
  #:use-module (dotf memo)
  #:use-module (fs-utils)
  #:use-module (command-line)
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
  #:use-module (ice-9 pretty-print)
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
  (let [(files '())]
    (define (file-collector filename statinfo flag base level)
      (when (equal? 'regular flag) ; it's a regular file
        (set! files (cons filename files)))
      #t)         ; continue traversing
    (nftw path file-collector)
    files))
(testsymb 'list-all-files)

(def (expand-pattern relative-dir pattern)
  "Examples:
(expand-pattern relative-dir \".*\")  ;; crep
(expand-pattern relative-dir \"cli/git\")
(expand-pattern relative-dir \"cli/\")
(expand-pattern relative-dir \"cvs\")
"
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
            (lambda (s) (string-match (basename re) s))))))))
(testsymb 'expand-pattern)

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

(def* (service-file-general
       #:key utility desc scm-file module-name chmod-params files
       (excluded-files (list))
       (other-files (list)))
  "The priority is 1. module-name, 2. scm-file, 3. utility
TODO The `search-notes' program should read a `search-space-file' containing
a list of files to search through.
Example:
    chmod --recursive u=rwx,g=rwx,o=rwx /path/to/dir
"
  ;; (format #t "~a Starting…\n" f)
  ;; (when (string=? utility "gicl")
  ;;   (format #t "~a git?         : ~s\n" f git?)
  ;;   (format #t "~a utility      : ~s\n" f utility)
  ;;   (format #t "~a desc         : ~s\n" f desc)
  ;;   (format #t "~a scm-file     : ~s\n" f scm-file)
  ;;   (format #t "~a module-name  : ~s\n" f module-name)
  ;;   (format #t "~a chmod-params : ~s\n" f chmod-params)
  ;;   (format #t "~a files        : ~s\n" f files)
  ;;   (format #t "~a other-files  : ~s\n" f other-files))
  (list
   (str scm-bin-dirname "/" utility)
   (program-file
    ;; 1st param: name
    (cond
     [(equal? scm-file "chmod")
      (str "chmod-plus-" chmod-params)]
     [(equal? scm-file "search-notes")
      (str "search-notes-" utility)]
     [#t
      desc])
    ;; 2nd param: exp
    ;; TODO clarify if source-module-closure is needed only for imports of
    ;; guix modules?
    (let* [(symb-string (or scm-file utility))
           (symb (or module-name
                     (string->symbol symb-string)))
           (main-call ((comp (partial remove unspecified?))
                       (list
                        'main
                        (cond
                         [(equal? scm-file "chmod")
                          `(let [(cmd-line (command-line))]
                             (append (list (car cmd-line)
                                           ,chmod-params)
                                     (cdr cmd-line)))]
                         [(equal? scm-file "search-notes")
                          `(append
                            (command-line)
                            (list
                             ;; set-difference of lists containing strings
                             ,@(s-
                                (append other-files (full-filepaths files))
                                (full-filepaths excluded-files))))]
                         [#t `(command-line)]))))]
      (with-imported-modules
          ((comp
            (partial remove unspecified?)
            (lambda (lst)
              (cond
               [(equal? scm-file "search-notes")
                (append lst `(
                              (guix profiling)
                              (guix memoization)
                              (guix colors)
                              ;; (ice-9 getopt-long)
                              ))]
;;; Having '#:use-module (fs-utils)' in the (scm-bin guix-git-authenticate) module
;;; implies importing a number of additional (guix ...) modules. Alternative
;;; solution: use '(getenv "dgx")' instead of 'dgx' from fs-utils.
               [(equal? utility "guix-git-authenticate")
                (append lst `(
                              (fs-utils)
                              (guix gexp)
                              (guix store)
                              (guix utils)
;;; Having (guix config) probably causes:
;;;     warning: importing module (guix config) from the host
                              (guix config)
                              (guix memoization)
                              (guix profiling)
                              (guix diagnostics)
                              (guix colors)
                              (guix i18n)
                              (guix deprecation)
                              (guix serialization)
                              (guix records)
                              (guix base16)
                              (guix base32)
                              (guix derivations)
                              (guix combinators)
                              (guix sets)
                              ))]
               [#t lst])))
           `((guix monads)
             (dotf srfi-1-smart)
             (dotf utils)
             (dotf settings)
             (scm-bin ,symb)))
        #~(begin
            (use-modules (scm-bin #$symb))
            #$main-call))))))
(testsymb 'service-file-general)

(def* (service-file-utils
       #:key (trace #f) (verbose #t) utility fun exec-fun params extra-modules
       #:allow-other-keys #:rest args)
  "Create pairs like
  (\"scm-bin/g\"         \"/gnu/store/...\") ; for emacs CLI utils
  (\"scm-bin/mount-axa\" \"/gnu/store/...\") ; for mount utils

Example:
(service-file-utils #:trace         #t
                    #:fun           'cli-general-command
                    #:exec-fun      'exec-background
                    #:extra-modules '()
                    #:utility       \"techo\"
                    #:params        \"echo \\\"foo\\\"\")

(service-file-utils #:trace         #t
                    #:fun           'cli-general-command
                    #:exec-fun      'exec-foreground
                    #:extra-modules '()
                    #:utility       \"techo\"
                    #:params        \"echo \\\"foo\\\"\")

(service-file-utils #:trace         #t
                    #:fun           'cli-general-command
                    #:exec-fun      'exec-system
                    #:extra-modules '()
                    #:utility       \"techo\"
                    #:params        \"echo \\\"foo\\\"\")

(service-file-utils #:trace         #t
                    #:exec-fun      'exec-foreground
                    #:extra-modules '((emacs-common))
                    #:utility       \"r\" #:fun 'create-launcher
                    ;; #:utility       \"er\" #:fun 'set-editable
                    ;; #:utility       \"kr\" #:fun 'pkill-server
                    #:params develop)

TODO The `search-notes' program should read a `search-space-file' containing
a list of files to search through."
  ;; (format #t "~a Starting…\n" f)

  (when trace
    (format #t "~a #:trace         ~a ; ~a\n" f (pr-str-with-quote trace)         (test-type trace))
    (format #t "~a #:verbose       ~a ; ~a\n" f (pr-str-with-quote verbose)       (test-type verbose))
    (format #t "~a #:utility       ~a ; ~a\n" f (pr-str-with-quote utility)       (test-type utility))
    (format #t "~a #:fun           ~a ; ~a\n" f (pr-str-with-quote fun)           (test-type fun))
    (format #t "~a #:exec-fun      ~a ; ~a\n" f (pr-str-with-quote exec-fun)      (test-type exec-fun))
    (format #t "~a #:params        ~a ; ~a\n" f (pr-str-with-quote params)        (test-type params))
    (format #t "~a #:extra-modules ~a ; ~a\n" f (pr-str-with-quote extra-modules) (test-type extra-modules))
    (format #t "~a   args          ~a ; ~a\n" f (pr-str-with-quote args)          (test-type args))
    (format #t "\n"))

  (define common-modules '((srfi srfi-1)
                           (guix monads)
                           (dotf srfi-1-smart)
                           (dotf utils)
                           (dotf tests)
                           (dotf settings)
                           (cli-common)
                           (command-line)))
  (list
   (str scm-bin-dirname "/" utility)
   ((comp
     ;; (lambda (v) (format #t "~a 0 test-type : ~a\n" f (test-type v)) v)
     )
    (program-file
     utility
;;; (scheme-file name gexp (#:splice?) (#:guile) (#:set-load-path?))
;;; A procedure in module (guix gexp).
;;; Return an object representing the Scheme file NAME that contains GEXP.
;;; This is the declarative counterpart of 'gexp->file'.
     (let* [(symb-string scheme-file)
            (symb (or module-name
                      (string->symbol symb-string)))
            (new-args (remove-all-elements
                       args
                       (list
                        #:extra-modules ;; consumed by this procedure
                        )))
            (fixed-new-args (append-map
                             (lambda (k v)
                               (list k (if (symbol? v) `(quote ,v) v)))
                             (plist-keys new-args)
                             (plist-vals new-args)))
            (sexp
             `(begin
                (use-modules (ice-9 getopt-long)
                             (ice-9 regex)
                             ,@common-modules
                             ,@extra-modules)
                (handle-cli
                 ,@(if (member? #:trace   new-args) `() `(#:trace   ,trace))
                 ,@(if (member? #:verbose new-args) `() `(#:verbose ,verbose))
                 ,@fixed-new-args
                 (command-line))))]
       (when trace ;; (string=? "rgt" utility)
         (format #t "common-modules : ~a\n" common-modules)
         (format #t "extra-modules  : ~a\n" extra-modules)
         (format #t "sexp :\n~a\n" (pretty-print->string sexp))
         (format #t "\n"))
       (with-imported-modules (append common-modules extra-modules)
         #~#$sexp))))))
(testsymb 'service-file-utils)

(define crc-other-files
  (flatten
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
          "src/cljs/fdk/cmap/")))))

(define cre-other-files
  ((comp
    ;; (lambda (v) (format #t "~a 3\n" m) v)
    (partial apply append)
    ;; (lambda (v) (format #t "~a 2\n" m) v)
    (partial map (partial apply expand-pattern))
    ;; (lambda (v) (format #t "~a 1\n" m) v)
    )
   (list
    ;; (get-src guix) points to a spacemacs-distros
    (list (substring (get-src guix)
                     (string-length (str home "/")))
          "core/el")
    (list "dev/kill-buffers" "el")
    (list "dev/dotfiles" ".sp.*macs")
    (list "dev/jump-last" "el")
    (list "dev/tweaks" "el")
    (list "dev/farmhouse-light-mod-theme" "el"))))

(def (search-notes-service)
  ;; (format #t "~a Starting…\n" f)
  (map
   (partial apply service-file-general)
   (list
    (list #:utility "crc"
          #:files (list "lisp/clojure")
          #:other-files crc-other-files
          #:scm-file "search-notes")

    (list #:utility "cre"
          #:files (list "editors/")
          #:other-files cre-other-files
          #:scm-file "search-notes")
    (list #:utility "crep"
          #:excluded-files (list "notes.scrbl")
          #:files (list ".*")
          #:scm-file "search-notes")
    (list #:utility "cra"
          #:files (list "ai")
          #:scm-file "search-notes")
    (list #:utility "crf"
          #:files (list "cli/find_and_grep")
          #:scm-file "search-notes")
;;; TODO crg should also search in the $dotf/guix/
    (list #:utility "crg" #:files (list "guix-guile-nix/")
          #:other-files (append (expand-pattern "dev/guix" "scm"))
          #:scm-file "search-notes")
    (list #:utility "crgi" #:files (list "cli/git")
          #:other-files (append (expand-pattern "dev/dotfiles" ".gitconfig"))
          #:scm-file "search-notes")
;;; TODO crl should search in the $dotf/.config/fish and other profile files
    (list #:utility "crl" #:files (list "guix-guile-nix/" "cli/"
                                        ;; simple files
                                        "network" "cvs" "gui")
          #:other-files (append (expand-pattern "dev/dotfiles" ".bash"))
          #:scm-file "search-notes")
    (list #:utility "crli" #:files (list "cli/listing")
          #:scm-file "search-notes")
    (list #:utility "crr" #:files (list "lisp/racket")
          #:other-files (append (expand-pattern "der/search-notes" "rkt"))
          #:scm-file "search-notes")
;;; TODO create-def--emacs-<type>-<profile> crct - search in category-theory notes
;;; TODO crs should be like crl
    (list #:utility "crs" #:files (list "cli/shells")
          ;; #:other-files (append (expand-pattern "dev/dotfiles" ".bash"))
          #:scm-file "search-notes")
    (list #:utility "cru" #:files (list "utf8")
          #:scm-file "search-notes"))))
(testsymb 'search-notes-service)

(define (eza-command . args)
  (apply
   (partial format #f "eza ~a ~a"
            (string-join
             (list
              "-abghHliS"
              ;; "a" ;; this second 'a' also displays '..':
;;; $ exa -aabghHliS --color=always --time-style=full-iso /home/bost/.lein
;;; inode Permissions Links  Size Blocks User Group Date Modified                       Name
;;; 11844576 lrwxrwxrwx      1    29      0 bost users 2022-04-11 13:26:27.833652179 +0200 . -> /home/bost/dev/dotfiles/.lein
;;; 28578649 drwxr-xr-x     18     -      - bost users 2023-11-03 01:28:21.806608724 +0100 ..
;;; 28603148 .rw-r--r--      1 1.7Ki      8 bost users 2023-10-02 11:23:42.290304922 +0200 profiles.clj

;;; "d" ;; this displays the arrow -> '..' for the links, ...
;;; $ exa -daabghHliS --color=always --time-style=full-iso /home/bost/.lein
;;; inode Permissions Links Size Blocks User Group Date Modified                       Name
;;; 11844576 lrwxrwxrwx      1   29      0 bost users 2022-04-11 13:26:27.833652179 +0200 /home/bost/.lein -> /home/bost/dev/dotfiles/.lein
;;; ... however it doesn't list the content of directories:
;;; $ exa -daabghHliS --color=always --time-style=full-iso /home/bost
;;; inode Permissions Links Size Blocks User Group Date Modified                       Name
;;; 11796482 drwx------    115    -      - bost users 2023-11-03 19:51:27.577156745 +0100 /home/bost
              "--color=always" "--time-style=+%d-%m-%Y\\ %H:%M:%S")
             ))
   args))

(define (git-cmdstr . args) (apply (partial format #f "git -c color.ui=always ~a") args))
;; TODO implement git-cmdstr without colors
;; (define (git-cmdstr . args) (apply (partial format #f "git -c color.interactive=always ~a") args))
;; (def-public (git-cmdstr-no-color . args)
;;   (format #t "~a args: ~a\n" f args)
;;   (apply (partial format #f "git ~a") args))

(define ripgrep-utils-definition
  ((comp
    (partial map (partial
                  append
                  (list
                   ;; #:trace #t
                   ;; ripgrep returns 1 when nothing is found. Do not error out!
                   #:ignore-errors #t
                   ;; no verbosity for processing in the CLI pipeline
                   #:verbose #f
                   )))

    (partial
     append
     ((comp
       ;; (lambda (v) (format #t "~a 1 ~a\n" f v) v)
       (partial fold append (list))
       ;; (lambda (v) (format #t "~a 0 ~a\n" f v) v)
       (partial
        map
        (lambda (context-lines)
          (list
           (list #:utility (str "rg" context-lines)
                 #:params (str "rg --ignore-case --pretty"
                               " --context=" context-lines))
           (list #:utility (str "rgt" context-lines)
                 #:params (str "rg --ignore-case --pretty --type=lisp"
                               " --context=" context-lines))))))
      (list "2" "4" "6" "8"))))
   (list
    (list #:utility "rgt"
          #:params "rg --ignore-case --pretty --type=lisp"
          #:desc "Rigprep LISP files"))))

(define rest-utils-definitions
  (list
   (list #:utility "gx"  #:params "guix")

   ;; verbose with colors
   (list #:utility "f"   #:params "fd --color=always"
         #:verbose #t
         #:desc "Find entries in the filesystem")

   ;; no verbosity, no colors for processing in CLI pipeline by rigprep etc.
   (list #:utility "fc"  #:params "fd --color=never"
         #:verbose #f
         #:desc "Find entries in the filesystem. Suitable for CLI pipeline")

   ;; always lists to the end of file. I guess I need to use something else than `exec'
   (list #:utility "c"   #:params "bat --force-colorization"            #:desc "Better cat")
   (list #:utility "b"   #:params "bat --force-colorization"            #:desc "Better cat")
   (list #:utility "cl"  #:params "xsel --clipboard"                    #:desc "Show clipboard content")

   (list #:utility "l"   #:params (eza-command "")                      #:desc "List")
   (list #:utility "lh"  #:params (eza-command "--header")              #:desc "List with headers")
   (list #:utility "lT"  #:params (eza-command "--sort=time")           #:desc "List sorted by time, oldest on top")
   (list #:utility "lt"  #:params (eza-command "--sort=time --reverse") #:desc "List sorted by time, youngest on top")
;;; TODO lf: get the content of the current working directory, i.e emulate the
;;; globing expansion of "{*,.*}"
   (list #:utility "lf"  #:params ((comp
                                    eza-command
                                    (partial format #f "~a --list-dirs --oneline"))
                                   (getcwd))                            #:desc "List with full paths")


   ;; pwr and prw do the same (w and r are swapped)
   (list #:utility "pwr" #:params "chmod +rw")
   (list #:utility "prw" #:params "chmod +rw")
   (list #:utility "px"  #:params "chmod +x")

   (list #:utility "susp" #:params "xfce4-session-logout --suspend" #:desc "Suspend to RAM")
   ;; gifetare.fish:git fetch --tags … && git rebase …

;;; In bash a script is executes in a subshell, so the cd command only changes
;;; the directory within that subshell. So `gicl` for bash it it implemented as
;;; a function in .bashrc. See home-base.scm
   ;; TODO gicl needs special treatment
   (list #:utility "gicl"  #:params (git-cmdstr "clone"))
   (list #:utility "girt"  #:params (git-cmdstr "remote"))
   (list #:utility "girtv" #:params (git-cmdstr "remote --verbose"))
   (list #:utility "gire"  #:params (git-cmdstr "rebase"))
   (list #:utility "girea" #:params (git-cmdstr "rebase --abort"))
   (list #:utility "girec" #:params (git-cmdstr "rebase --continue"))
   (list #:utility "girei" #:params (git-cmdstr "rebase --interactive"))
   (list #:utility "gires" #:params (git-cmdstr "rebase --skip"))
   (list #:utility "gife"  #:params (git-cmdstr "fetch"))
   (list #:utility "gico"  #:params (git-cmdstr "checkout"))
   (list #:utility "gico-" #:params (git-cmdstr "switch -"))
   (list #:utility "gicm"  #:params (git-cmdstr "checkout master"))
   (list #:utility "gips"  #:params (git-cmdstr "push --verbose"))
   (list #:utility "gipsf" #:params (git-cmdstr "push --verbose --force"))
   (list #:utility "giad"  #:params (git-cmdstr "add"))
   (list #:utility "giad." #:params (git-cmdstr "add ."))
   (list #:utility "giap"  #:params (git-cmdstr "add --patch"))
   (list #:utility "gibib" #:params (git-cmdstr "bisect bad"))
   (list #:utility "gibig" #:params (git-cmdstr "bisect good"))
   (list #:utility "gibir" #:params (git-cmdstr "bisect reset"))
   (list #:utility "gibr"  #:params (git-cmdstr "branch"))
   (list #:utility "gibrD" #:params (git-cmdstr "branch --force --delete"))
   (list #:utility "gibra" #:params (git-cmdstr "branch --all"))
   (list #:utility "gibrd" #:params (git-cmdstr "branch --delete"))
   (list #:utility "gibrm" #:params (git-cmdstr "branch --move"))
   (list #:utility "gici"  #:params (git-cmdstr "commit"))
   (list #:utility "gicia" #:params (git-cmdstr "commit --amend"))
   (list #:utility "gicmanoe" #:params (git-cmdstr "commit --amend --no-edit"))
   (list #:utility "gicob"  #:params (git-cmdstr "checkout -b"))
   (list #:utility "gicp"   #:params (git-cmdstr "cherry-pick"))
   (list #:utility "gidf"   #:params (git-cmdstr "diff --word-diff delete"))
   (list #:utility "gifeo"  #:params (git-cmdstr "fetch origin"))
   (list #:utility "gifeu"  #:params (git-cmdstr "fetch upstream"))
   (list #:utility "gila"   #:params (git-cmdstr "lg-all"))
   (list #:utility "gilh"   #:params (git-cmdstr "lg-head"))
   (list #:utility "gilo"   #:params (git-cmdstr "log"))
   (list #:utility "gimv"   #:params (git-cmdstr "mv"))
   (list #:utility "gipS"   #:params (git-cmdstr "push --force --verbose"))
   (list #:utility "gipl"   #:params (git-cmdstr "pull"))
   (list #:utility "giplr"  #:params (git-cmdstr "pull --rebase --verbose"))
   (list #:utility "girm"   #:params (git-cmdstr "rm"))
   (list #:utility "girsth" #:params (git-cmdstr "reset --hard"))
   (list #:utility "gishp"  #:params (git-cmdstr "stash pop"))
   (list #:utility "gishs"  #:params (git-cmdstr "stash save"))
   (list #:utility "gist"   #:params (git-cmdstr "status"))
   (list #:utility "gists"  #:params (git-cmdstr "status --short"))
   (list #:utility "gita"   #:params (git-cmdstr "tag --sort version:refname"))
   ;; #:desc "Show last 20 git logs: git lg-20 …'"
   (list #:utility "lg"     #:params (git-cmdstr "lg-20"))
   (list #:utility "lga"    #:params (git-cmdstr "lg"))

   ;;  -c <name>=<value>
   ;; Pass a configuration parameter to the command. The value given will
   ;; override values from configuration files. The <name> is expected in the
   ;; same format as listed by git config (subkeys separated by dots).
   (list #:utility "gs"     #:params (git-cmdstr "status"))
   (list #:utility "wp"     #:params "printf '\\ec'" #:desc "Wipe / clear terminal")
   ))
(testsymb 'rest-utils-definitions)

(def* (basic-cli-utils-service)
  ;; (call/cc (lambda (exit)))
  ;; (format #t "~a Starting…\n" f)
  ((comp
    ;; (lambda (v) (format #t "~a done\n" f) v)
    (partial
     map
     (comp
      (partial apply service-file-utils)
      (partial append (list #:fun 'cli-general-command
                            #:exec-fun 'exec-foreground
                            #:extra-modules '())))))
   (append
    ripgrep-utils-definition
    rest-utils-definitions)))
(testsymb 'basic-cli-utils-service)

(def (basic-cli-utils-background-service)
  ;; (call/cc (lambda (exit)))
  ((comp
    (partial map (comp
                  (partial apply service-file-utils)
                  (partial append (list
                                   #:fun 'cli-general-command
                                   #:exec-fun 'exec-background
                                   #:extra-modules '())))))
   (list
    ;; WTF? a newline appears on top of the terminal before the prompt.
    (list #:utility "loff"   #:params "xfce4-session-logout --logout --fast")
    )))
(testsymb 'basic-cli-utils-background-service)

(def (sudo-cli-utils-service)
  ;; (call/cc (lambda (exit)))
  ((comp
    (partial map (comp
                  (partial apply service-file-utils)
                  (partial append (list
                                   #:fun 'cli-general-command
                                   #:exec-fun 'exec-system
                                   #:extra-modules '())))))
   (list
    (list #:utility "shut"   #:params "sudo shutdown")
    ;; scm-bin/reboot overshadows the real reboot in the $PATH
    (list #:utility "reboot" #:params "sudo /run/current-system/profile/sbin/reboot")
    )))
(testsymb 'basic-cli-utils-background-service)

(define (mount-utils-service)
  ((comp
    (partial map (comp
                  (partial apply service-file-utils)
                  (lambda (disk-pair)
                    (let [(disk-operation (car disk-pair))
                          (disk-label     (cadr disk-pair))]
                      (list
                       #:fun disk-operation
                       #:exec-fun 'exec-foreground
                       #:params disk-label
                       #:utility (str disk-operation "-" disk-label)
                       #:extra-modules '((mount-common))))))))
   (cartesian
    (list 'mount 'unmount 'eject 'info)
    (list "axa" "toshiba" "new" "t7"))))
(testsymb 'mount-utils-service)

(define (emacs-cli-utils-service)
  ((comp
    (partial map (comp
                  (partial apply service-file-utils)
                  (partial append (list
                                   #:exec-fun 'exec-foreground
                                   #:extra-modules '((emacs-common)))))))
   (list
    (list #:utility  "d" #:fun 'create-launcher  #:params develop)
    (list #:utility "ed" #:fun 'set-editable     #:params develop)
    (list #:utility "kd" #:fun 'pkill-server     #:params develop)

    (list #:utility  "cy" #:fun 'create-launcher #:params cycle)
    (list #:utility "ecy" #:fun 'set-editable    #:params cycle)
    (list #:utility "kcy" #:fun 'pkill-server    #:params cycle)

    (list #:utility  "g" #:fun 'create-launcher  #:params guix)
    (list #:utility "eg" #:fun 'set-editable     #:params guix)
    (list #:utility "kg" #:fun 'pkill-server     #:params guix)

    (list #:utility  "s" #:fun 'create-launcher  #:params spguix)
    (list #:utility "es" #:fun 'set-editable     #:params spguix)
    (list #:utility "ks" #:fun 'pkill-server     #:params spguix)

    (list #:utility  "r" #:fun 'create-launcher  #:params crafted)
    ;; TODO Move crafted-emacs user config from the project repo to the dotfiles
    ;; (list #:utility "er" #:fun 'set-editable  #:params crafted)
    (list #:utility "kr" #:fun 'pkill-server     #:params crafted)
    )))
(testsymb 'emacs-cli-utils-service)

(define (direct-utils-service)
  (map (partial apply service-file-general)
       (list
        (list #:utility "extract"               #:desc "extract-uncompress")
        ;; gg and gk call exec-background
        (list #:utility "gg"                    #:desc "git-gui")
        (list #:utility "gk"                    #:desc "git-repo-browser")
        (list #:utility "guix-git-authenticate" #:desc "guix-git-authenticate")
        (list #:utility "gpg-pinentry-setup"    #:desc "gpg-pinentry-setup")
        (list #:utility "qemu-vm"               #:desc "qemu-vm")
        )))
(testsymb 'direct-utils-service)

(def-public (cli-utils-service)
  ;; (format #t "~a Starting…\n" f)
  ((comp
    ;; (lambda (v) (format #t "~a done\n" f) v)
    ;; 'simple-service name target value'. E.g.:
    ;; (simple-service 'my-mcron-job mcron-service-type #~(job '(next-hour (3)) "guix gc -F 2G"))
    (partial simple-service 'basic-cli-utils-service home-files-service-type))
   (if (or (host-ecke?) (host-edge?))
       (append
        (search-notes-service)
        (basic-cli-utils-service)
        (basic-cli-utils-background-service)
        (sudo-cli-utils-service)
        (mount-utils-service)
        (emacs-cli-utils-service)
        (direct-utils-service)
        )
       (list))))
(testsymb 'cli-utils-service)

(module-evaluated)
