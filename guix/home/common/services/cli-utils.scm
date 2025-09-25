(define-module (services cli-utils)
  #:use-module (srfi-1-smart)
  #:use-module (utils)
  #:use-module (tests)
  #:use-module (settings)
  #:use-module (memo)
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
  (let [(files '())]
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
  (define f (format #f "~a [expand-pattern]" m))
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

(define* (service-file-general
          #:key
          utility desc scm-file module-name
          chmod-params files
          (other-files (list)))
  "The priority is 1. module-name, 2. scm-file, 3. utility
TODO The `search-notes' program should read a `search-space-file' containing
a list of files to search through.
Example:
    chmod --recursive u=rwx,g=rwx,o=rwx /path/to/dir
"
  (define f (format #f "~a [service-file-general]" m))
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
                            (list ,@(append other-files
                                            (full-filepaths files))))]
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
             (srfi-1-smart)
             (utils)
             (settings)
             (scm-bin ,symb)))
        #~(begin
            (use-modules (scm-bin #$symb))
            #$main-call))))))
(testsymb 'service-file-general)

(define* (service-file-utils
          #:key utility (verbose #f) fun exec-fun params extra-modules
          #:allow-other-keys)
  "Create pairs like
  (\"scm-bin/g\" \"/gnu/store/...\")         ; for emacs CLI utils
  (\"scm-bin/mount-axa\" \"/gnu/store/...\") ; for mount utils

TODO The `search-notes' program should read a `search-space-file' containing
a list of files to search through."
  (define f (format #f "~a [service-file-utils]" m))
  ;; (format #t "~a Starting…\n" f)
  ;; (format #t "~a utility : ~s\n" f utility)
  ;; (format #t "~a fun     : ~s\n" f fun)
  ;; (format #t "~a profile : ~s\n" f profile)
  (define common-modules '((srfi srfi-1)
                           (guix monads)
                           (srfi-1-smart)
                           (utils)
                           (tests)
                           (settings)
                           (command-line)))
  (list
   (str scm-bin-dirname "/" utility)
   ((comp) ;; logger stub
    (program-file
     utility
;;; (scheme-file name gexp (#:splice?) (#:guile) (#:set-load-path?))
;;; A procedure in module (guix gexp).
;;; Return an object representing the Scheme file NAME that contains GEXP.
;;; This is the declarative counterpart of 'gexp->file'.
     (let* [(symb-string scheme-file)
            (symb (or module-name
                      (string->symbol symb-string)))
            (sexp `(handle-cli #:verbose ,verbose
                               #:utility ,utility
                               #:fun (quote ,fun)
                               #:exec-fun (quote ,exec-fun)
                               ,@(if params `(#:params ,params) '())
                               (command-line)))]
       (with-imported-modules (append common-modules extra-modules)
         #~(begin
             (use-modules (ice-9 getopt-long)
                          (ice-9 regex)
                          #$@common-modules
                          #$@extra-modules)
             #$sexp)))))))
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

(define (search-notes-service)
  (define f (format #f "~a [search-notes-service]" m))
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
          #:files (list ".*") ;; TODO exclude /home/bost/org-roam/notes.scrbl
          #:scm-file "search-notes")
    (list #:utility "cra"
          #:files (list "ai")
          #:scm-file "search-notes")
    (list #:utility "crf"
          #:files (list "cli/find_and_grep")
          #:scm-file "search-notes")
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
    (list #:utility "crg" #:files (list "guix-guile-nix/")
          #:other-files (append (expand-pattern "dev/guix" "scm"))
          #:scm-file "search-notes")
;;; TODO crgi should also search in the output of `git config --get' etc.
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

(define (git-command . args) (apply (partial format #f "git -c color.ui=always ~a") args))
;; TODO implement git-command without colors
;; (define (git-command . args) (apply (partial format #f "git -c color.interactive=always ~a") args))
;; (define-public (git-command-no-color . args)
;;   (define f (format #f "~a [git-command-no-color]" m))
;;   (format #t "~a args: ~a\n" f args)
;;   (apply (partial format #f "git ~a") args))

(define (ripgrep-utils context-lines)
  (list
   (list
    #:utility (str "rg" context-lines)
    #:params
    (str "rg --ignore-case --pretty --context=" context-lines))
   (list
    #:utility (str "rgt" context-lines)
    #:params
    (str "rg --ignore-case --pretty --type=lisp --context=" context-lines))))

(define utils-definitions
  (list
   (list #:utility "gx"  #:params "guix")

   (list #:utility "rgt" #:params "rg --ignore-case --pretty --type=lisp" #:desc "Rigprep LISP files")
   (list #:utility "f"   #:params "fd --color=always"                   #:desc "Find entries in the filesystem")

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
   (list #:utility "gicl"  #:params (git-command "clone"))
   (list #:utility "girt"  #:params (git-command "remote"))
   (list #:utility "girtv" #:params (git-command "remote --verbose"))
   (list #:utility "gire"  #:params (git-command "rebase"))
   (list #:utility "girea" #:params (git-command "rebase --abort"))
   (list #:utility "girec" #:params (git-command "rebase --continue"))
   (list #:utility "girei" #:params (git-command "rebase --interactive"))
   (list #:utility "gires" #:params (git-command "rebase --skip"))
   (list #:utility "gife"  #:params (git-command "fetch"))
   (list #:utility "gico"  #:params (git-command "checkout"))
   (list #:utility "gico-" #:params (git-command "switch -"))
   (list #:utility "gicm"  #:params (git-command "checkout master"))
   (list #:utility "gips"  #:params (git-command "push --verbose"))
   (list #:utility "gipsf" #:params (git-command "push --verbose --force"))
   (list #:utility "giad"  #:params (git-command "add"))
   (list #:utility "giad." #:params (git-command "add ."))
   (list #:utility "giap"  #:params (git-command "add --patch"))
   (list #:utility "gibib" #:params (git-command "bisect bad"))
   (list #:utility "gibig" #:params (git-command "bisect good"))
   (list #:utility "gibir" #:params (git-command "bisect reset"))
   (list #:utility "gibr"  #:params (git-command "branch"))
   (list #:utility "gibrD" #:params (git-command "branch --force --delete"))
   (list #:utility "gibra" #:params (git-command "branch --all"))
   (list #:utility "gibrd" #:params (git-command "branch --delete"))
   (list #:utility "gibrm" #:params (git-command "branch --move"))
   (list #:utility "gici"  #:params (git-command "commit"))
   (list #:utility "gicia" #:params (git-command "commit --amend"))
   (list #:utility "gicmanoe" #:params (git-command "commit --amend --no-edit"))
   (list #:utility "gicob"  #:params (git-command "checkout -b"))
   (list #:utility "gicp"   #:params (git-command "cherry-pick"))
   (list #:utility "gidf"   #:params (git-command "diff --word-diff delete"))
   (list #:utility "gifeo"  #:params (git-command "fetch origin"))
   (list #:utility "gifeu"  #:params (git-command "fetch upstream"))
   (list #:utility "gila"   #:params (git-command "lg-all"))
   (list #:utility "gilh"   #:params (git-command "lg-head"))
   (list #:utility "gilo"   #:params (git-command "log"))
   (list #:utility "gimv"   #:params (git-command "mv"))
   (list #:utility "gipS"   #:params (git-command "push --force --verbose"))
   (list #:utility "gipl"   #:params (git-command "pull"))
   (list #:utility "giplr"  #:params (git-command "pull --rebase"))
   (list #:utility "girm"   #:params (git-command "rm"))
   (list #:utility "girsth" #:params (git-command "reset --hard"))
   (list #:utility "gishp"  #:params (git-command "stash pop"))
   (list #:utility "gishs"  #:params (git-command "stash save"))
   (list #:utility "gist"   #:params (git-command "status"))
   (list #:utility "gists"  #:params (git-command "status --short"))
   (list #:utility "gita"   #:params (git-command "tag --sort version:refname"))
   ;; #:desc "Show last 20 git logs: git lg-20 …'"
   (list #:utility "lg"     #:params (git-command "lg-20"))
   (list #:utility "lga"    #:params (git-command "lg"))

   ;;  -c <name>=<value>
   ;; Pass a configuration parameter to the command. The value given will
   ;; override values from configuration files. The <name> is expected in the
   ;; same format as listed by git config (subkeys separated by dots).
   (list #:utility "gs"       #:params (git-command "status"))
   (list #:utility "wp"       #:params "printf '\\ec'" #:desc "Wipe / clear terminal")
   ))
(testsymb 'utils-definitions)

(define (basic-cli-utils-service)
  (define f (format #f "~a [basic-cli-utils-service]" m))
  ;; (call/cc (lambda (exit)))
  ((comp
    (partial map (comp
                  (partial apply service-file-utils)
                  (partial append (list #:verbose #f
                                        #:fun 'cli-command
                                        #:exec-fun 'exec-foreground
                                        #:extra-modules '((cli-common))))))
    (partial
     append
     ((comp
       ;; (lambda (v) (format #t "~a 1 ~a\n" f v) v)
       (partial fold append (list))
       ;; (lambda (v) (format #t "~a 0 ~a\n" f v) v)
       (partial map ripgrep-utils))
      (list "2" "4" "6" "8"))))
   utils-definitions))
(testsymb 'basic-cli-utils-service)

(define (basic-cli-utils-background-service)
  (define f (format #f "~a [basic-cli-utils-background-service]" m))
  ;; (call/cc (lambda (exit)))
  ((comp
    (partial map (comp
                  (partial apply service-file-utils)
                  (partial append (list #:verbose #f
                                        #:fun 'cli-background-command
                                        #:exec-fun 'exec-background
                                        #:extra-modules '((cli-common)))))))
   (list
    ;; WTF? a newline appears on top of the terminal before the prompt.
    (list #:utility "loff"   #:params "xfce4-session-logout --logout --fast")
    )))
(testsymb 'basic-cli-utils-background-service)

(define (sudo-cli-utils-service)
  (define f (format #f "~a [sudo-cli-utils-service]" m))
  ;; (call/cc (lambda (exit)))
  ((comp
    (partial map (comp
                  (partial apply service-file-utils)
                  (partial append (list #:verbose #f
                                        #:fun 'cli-system-command
                                        #:exec-fun 'exec-system
                                        #:extra-modules '((cli-common)))))))
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
                  (lambda (fun-label-pair)
                    (let [(fun (car fun-label-pair))
                          (lbl (cadr fun-label-pair))]
                      (list #:verbose #f
                            #:fun fun
                            #:exec-fun 'exec-foreground
                            #:params lbl
                            #:utility (str fun "-" lbl)
                            #:extra-modules '((mount-common))))))))
   (cartesian
    (list 'mount 'unmount 'eject)
    (list "axa" "toshiba" "new"))))
(testsymb 'mount-utils-service)

(define (emacs-cli-utils-service)
  ((comp
    (partial map (comp
                  (partial apply service-file-utils)
                  (partial append (list #:verbose #f
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

(define-public (cli-utils-service)
  (define f (format #f "~a [cli-utils-service]" m))
  ;; (format #t "~a Starting…\n" f)
  ((comp
    ;; (lambda (v) (format #t "~a done\n" f) v)
    ;; 'simple-service name target value'. E.g.:
    ;; (simple-service 'my-mcron-job mcron-service-type #~(job '(next-hour (3)) "guix gc -F 2G"))
    (partial simple-service 'basic-cli-utils-service home-files-service-type))
   (if (or (is-system-ecke) (is-system-edge))
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
