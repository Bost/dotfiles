(define-module (scm-bin ls)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  #:export (main ls))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ ls) -s
!#

cd $dotf
./guix/home/common/scm-bin/ls.scm /home/bost/.lein

|#

(define m (module-name-for-logging))
(evaluating-module)

(define* (ls #:rest args)
  "Usage:
(ls \"-f\" \"arg0\")
(ls \"-f arg0\")
(equal? (ls \"-f\" \"arg0\")
        (ls \"-f arg0\"))
;; > #t
"
  (apply exec-system*
         "eza"
          (str
            "-abghHliS"
            ;; "a" ;; this second 'a' also displays '..':
;;; $ exa -aabghHliS --color=always --time-style=full-iso /home/bost/.lein
;;; inode Permissions Links  Size Blocks User Group Date Modified                       Name
;;; 11844576 lrwxrwxrwx      1    29      0 bost users 2022-04-11 13:26:27.833652179 +0200 . -> /home/bost/dev/dotfiles/.lein
;;; 28578649 drwxr-xr-x     18     -      - bost users 2023-11-03 01:28:21.806608724 +0100 ..
;;; 28603148 .rw-r--r--      1 1.7Ki      8 bost users 2023-10-02 11:23:42.290304922 +0200 profiles.clj

            ;; "d" ;; this displays the arrow -> '..' for the links, ...
;;; $ exa -daabghHliS --color=always --time-style=full-iso /home/bost/.lein
;;; inode Permissions Links Size Blocks User Group Date Modified                       Name
;;; 11844576 lrwxrwxrwx      1   29      0 bost users 2022-04-11 13:26:27.833652179 +0200 /home/bost/.lein -> /home/bost/dev/dotfiles/.lein
;;; ... however it doesn't list the content of directories:
;;; $ exa -daabghHliS --color=always --time-style=full-iso /home/bost
;;; inode Permissions Links Size Blocks User Group Date Modified                       Name
;;; 11796482 drwx------    115    -      - bost users 2023-11-03 19:51:27.577156745 +0100 /home/bost
            )
         "--color=always" "--time-style=full-iso"
         #|
         "eza" "-abghHliS" "--color=always"
         ;; exa has no support for '+%d-%m-%Y %H:%M:%S' time formatters
         "eza" "-abghHliS" "--color=always" "--time-style=default"
         "eza" "-abghHliS" "--color=always" "--time-style=iso"
         "eza" "-abghHliS" "--color=always" "--time-style=long-iso"
         ;; '--file-type' append indicator (one of /=>@|) to entries
         ;; TODO consider custom coloring after `ls --color=never`
         "ls" "-lA" "--file-type" "--color"
         "--time-style=+%d-%m-%Y %H:%M:%S"
         |#
         args))

(define* (main #:rest args)
  "Usage:
(main \"<ignored>\" \"-f\" \"arg0\")"
  ((comp
    (partial apply ls)
    (partial apply cdr)
    #;dbg)
   args))
(testsymb 'main)

(module-evaluated)
