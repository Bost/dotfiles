(define-module (scm-bin search-notes)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #:use-module (srfi srfi-1)       ; last
  #:use-module (guix colors)
  #:use-module (utils)
  ;; #:use-module (guix build utils) ; invoke
  #:export (main search-notes))

#|
;; `-e (module)` calls the `main` from a given module or `-e my-procedure` calls
;; `my-procedure` from current module

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ search-notes) -s
!#

cd $dotf
./guix/home/common/scm-bin/search-notes.scm \
    'rest ' \
    /home/bost/org-roam/guix-guile-nix/guile.scrbl \
    /home/bost/org-roam/guix-guile-nix/guile_scripting.scrbl
|#

(define m
  #;(module-name-for-logging)
  ((comp
    (partial string-join)
    (partial map (partial format #f "~a"))
    (partial module-name))
   (current-module)))
(evaluating-module)

(define dbg #f)
(define utility-name (last (module-name (current-module))))

;; (define diacritic-map
;;   (hash "a" "[aáäàâæ]"
;;         "c" "[cčç]"
;;         "d" "[dď]"
;;         "e" "[eéèêë]"
;;         "i" "[iíîï]"
;;         "l" "[lĺľ]"
;;         "n" "[nň]"
;;         "o" "[oóôöœ]"
;;         "r" "[rŕř]"
;;         "s" "[sš]"
;;         "t" "[tť]"
;;         "u" "[uúûüù]"
;;         "y" "[yý]"
;;         "z" "[zž]"
;;         "A" "[AÁÄÀÂÆ]"
;;         "C" "[CČÇ]"
;;         "D" "[DĎ]"
;;         "E" "[EÉÈÊË]"
;;         "I" "[IÍÎÏ]"
;;         "L" "[LĹĽ]"
;;         "N" "[NŇ]"
;;         "O" "[OÓÔÖŒ]"
;;         "R" "[RŔŘ]"
;;         "S" "[SŠ]"
;;         "T" "[TŤ]"
;;         "U" "[UÚÛÜÙ]"
;;         "Y" "[YÝ]"
;;         "Z" "[ZŽ]"
;;         "ß" "ß")) ; German sharp S
;;
;; (define (string-normalize s)
;;   ;; Normalization Form C, Canonical Decomposition followed by Canonical
;;   ;; Composition:
;;   ;; Decompose characters and then recomposes them using canonical
;;   ;; equivalence. E.g., 'é' would first be split into 'e' and the combining
;;   ;; accent, and then recomposed back into 'é'.
;;   ;; Use this when you want to normalize characters to their composed forms
;;   ;; while still respecting canonical equivalence.
;;   (string-normalize-nfc s))
;;
;; (define (regexp-normalize-match* regex target-str)
;;   ;; (printf "[regexp-normalize-match*] regex: ~a\n" regex)
;;   ;; (printf "[regexp-normalize-match*] target-str : ~a\n" target-str)
;;   (let* ((normalized-target (string-normalize target-str)))
;;     ;; (printf "[regexp-normalize-match*] normalized-target: ~a\n" normalized-target)
;;     (regexp-match* regex normalized-target)))
;;
;; (define (regexp-normalize-split regex target-str)
;;   ;; (printf "[regexp-normalize-split] regex: ~a\n" regex)
;;   ;; (printf "[regexp-normalize-split] target-str : ~a\n" target-str)
;;   (let* ((normalized-target (string-normalize target-str)))
;;     ;; (printf "[regexp-normalize-split] normalized-target: ~a\n" normalized-target)
;;     (regexp-split regex normalized-target)))

(define (search-file pattern file)
  "
(search-file \"pattern\"
             \"/home/bost/dev/notes/notes/bric_a_brac.scrbl\")
"
  ;; Red: \033[31m
  ;; Bright Red: \033[1;31m
  ;; Green: \033[32m
  ;; Yellow: \033[33m
  ;; Blue: \033[34m
  ;; Magenta: \033[35m
  ;; Cyan: \033[36m
  ;; White: \033[37m
  ;; Reset: \033[0m
  (let* [
         ;; If the line is empty or contains only whitespace chars then don't
         ;; prefix it. Else prefix it with '<the-line-number>:' and
         ;; <the-line-number> is displayed Green
         (cmd1
          (format #f
                  "awk '
/^[[:space:]]*$/ {
    print; next
}
{
    printf \"\\033[32m%d\\033[0m:  %s\\n\", NR, $0
}
' '~a'" file))
         (cmd2 "sed 's/^\\s*$//'")

         (cmd3
          (format
           #f
           ;; -v var=val		--assign=var=val
           "awk -v RS='' '/~a/ {gsub(/~a/, \"\\033[1;31m&\\033[0m\"); print $0 \"\\n\"}'"
           pattern pattern))
         (cmd (format #f "~a | ~a | ~a" cmd1 cmd2 cmd3))
         (ret (exec cmd #:verbose #f))]
    (if (= 0 (car ret))
        (let* [(output (cdr ret))]
          (unless (null? output)
            (format #t "~a\n" (colorize-string file (color MAGENTA))) ; file in magenta
            (map (partial format #t "~a\n") output)))
        (error-command-failed m))))
(testsymb 'search-file)

(define* (search-notes #:rest args)
  "Usage:
(search-notes (list \"<ignored>\"
  \"rest \"
  \"/home/bost/org-roam/guix-guile-nix/guile.scrbl\"
  \"/home/bost/org-roam/guix-guile-nix/guile_scripting.scrbl\"))
"
  ;; (format #t "args: '~a'\n" args)
  (let* [(arg-lst (car args))
         (pattern ((comp car cdr) arg-lst))]
    ;; (format #t "arg-lst: '~a'\n" arg-lst)
    ;; (format #t "ptrn: '~a'\n" pattern)
    (let* [(files ((comp cdr cdr) arg-lst))]
      ;; (format #t "files: '~a'\n" files)
      (map (partial search-file pattern) files))))

(define main search-notes)

(testsymb 'main)

(module-evaluated)
