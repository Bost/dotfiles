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
  #:export (
            scheme-files-service
            ))

(define m (module-name-for-logging))
;; (format #t "~a evaluating module ...\n" m)

(define* (service-file #:key
                       program-name desc scheme-file-name module-name
                       chmod-params files)
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
                                files))
                             (command-line)))))
        (with-imported-modules
            (remove
             unspecified?
             `((utils)
               (settings)

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

(define search-notes-service-files
  (list
;;; TODO crc should search in the $dec
   (service-file #:program-name "crc"  #:files "clojure"
                 #:scheme-file-name "search-notes")
;;; TODO cre should also search in the ~/.emacs.d.spacemacs/, ~/.spacemacs, kill-buffers
;;; and my=tweaks, farmhouse-light-mod
   (service-file #:program-name "cre"  #:files "vim|emacs|org_mode"
                 #:scheme-file-name "search-notes")
   (service-file #:program-name "crep" #:files ".*"
                 #:scheme-file-name "search-notes")
   (service-file #:program-name "cra"  #:files "ai"
                 #:scheme-file-name "search-notes")
   (service-file #:program-name "crf"  #:files "find_and_grep"
                 #:scheme-file-name "search-notes")
;;; TODO crg should also search in the $dotf/guix/
   (service-file #:program-name "crg"  #:files "guix|guile|nix"
                 #:scheme-file-name "search-notes")
;;; TODO crgi should also search in the output of `git config --get',
;;; ~/.gitconfig, etc.
   (service-file #:program-name "crgi" #:files "git"
                 #:scheme-file-name "search-notes")
;;; TODO crl should search in the $dotf/.config/fish .bashrc, .bash_profile (and
;;; other profile files), etc.
   (service-file #:program-name "crl"
                 #:files (str "guix|guile|nix|shells|linux|network|android|cvs|"
                              "systemd|heroku|gui|packaging|rsync|listing")
                 #:scheme-file-name "search-notes")
   (service-file #:program-name "crli" #:files "listing"
                 #:scheme-file-name "search-notes")
;;; TODO crr should also search in the $der
   (service-file #:program-name "crr"  #:files "racket"
                 #:scheme-file-name "search-notes")
;;; TODO crs should be like crl
   (service-file #:program-name "crs"  #:files "shells"
                 #:scheme-file-name "search-notes")
   (service-file #:program-name "cru"  #:files "utf8"
                 #:scheme-file-name "search-notes")))

(define scheme-files-service
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
      [(is-system-ecke)
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
    (service-file #:program-name "gre" #:desc "git-remote")
    (service-file #:program-name "gfe" #:desc "git-fetch")
    (service-file #:program-name "gco" #:desc "git-checkout")
    (service-file #:program-name "gcod"#:desc "git-checkout-prev-branch")
    (service-file #:program-name "gcom"#:desc "git-checkout-master")
    (service-file #:program-name "gg"  #:desc "git-gui")
    ;; former ghog
    (service-file #:program-name "gps" #:desc "git-push")
    (service-file #:program-name "gk"  #:desc "git-repo-browser")
    ;; former glo
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

;; (format #t "~a module evaluated\n" m)
