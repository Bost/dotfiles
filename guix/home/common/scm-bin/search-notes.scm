(define-module (scm-bin search-notes)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (utils)
  ;; #:use-module (guix build utils) #| invoke |#
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ search-notes) -s
!#

cd $dotf
./guix/home/common/scm-bin/search-notes.scm 

|#

(define m (module-name-for-logging))
(evaluating-module)

(define (search-file ptrn file)
  "
(search-file
 \"/home/bost/dev/notes/notes/bric_a_brac.scrbl\"
 \"pattern\")
"
  (let* [(cmd1 (format #f "nl --body-numbering=t '~a'" file))
         (cmd2 "sed 's/^\\s*$//'")
         (cmd3
          ;; Red: \033[31m
          ;; Bright Red: \033[1;31m
          ;; Green: \033[32m
          ;; Yellow: \033[33m
          ;; Blue: \033[34m
          ;; Magenta: \033[35m
          ;; Cyan: \033[36m
          ;; White: \033[37m
          ;; Reset: \033[0m
          (format
           #f
           "awk -v RS='' '/~a/ {gsub(/~a/, \"\\033[1;31m&\\033[0m\"); print $0 \"\\n\"}'"
           ptrn ptrn))
         (ret (exec (format #f "~a | ~a | ~a" cmd1 cmd2 cmd3)))]
    (if (= 0 (car ret))
        (let* [(output (cdr ret))]
          #| process output |#
          (map (partial format #t "~a\n") output))
        (error-command-failed m))))
(testsymb 'search-file)

(define (main files args)
  "
(main
  (list \"/home/bost/dev/notes/notes/network.scrbl\"
        \"/home/bost/dev/notes/notes/bric_a_brac.scrbl\")
  (list \"_\" \"pattern\"))
"
  (let* [(ptrn ((comp car cdr) args))]
    (map (partial search-file ptrn) files)))
(testsymb 'main)
