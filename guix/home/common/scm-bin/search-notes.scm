(define-module (scm-bin search-notes)
;;; All used modules must be present in (@(services cli-utils) common-modules)
  #:use-module (guix colors)
  #:use-module (bost common utils)
  #:use-module (srfi srfi-1)       ; list-processing procedures
  #:use-module (ice-9 optargs)     ; define*-public
  #:use-module (ice-9 getopt-long) ; -v/--verbose parsing
  #:use-module (srfi srfi-26)      ; Conveniently specialize selected parameters
  )

#|
;; `-e (module)` calls the `main` from a given module or `-e my-procedure` calls
;; `my-procedure` from current module

#!/usr/bin/env -S guix repl --
!#

cd $dotf
# not '(apply main (command-line))'
echo -e "\n(main (command-line))" >> ./guix/home/common/scm-bin/search-notes.scm
./guix/home/common/scm-bin/search-notes.scm -v 'timezone ' \
  /home/bost/org-roam/cli/git.scrbl

|#

(define m (module-name-for-logging))

(evaluating-module)

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

(define* (search-file-ripgrep search-pattern file #:key (verbose #f))
  "
(search-file-ripgrep \"search-pattern\"
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
/^[[:space:]]*$/ { print; next }
{ printf \"\\033[32m%d\\033[0m:  %s\\n\", NR, $0 }
' '~a'" file))

         ;; Filter blocks containing the search-pattern
         (cmd2 "sed 's/^\\s*$//'")

         ;; Color the search-pattern
         (cmd3
          (format
           #f
           ;; "rg -IN --colors 'match:bg:0,128,255' ~a"

           ;; -U / --multiline allows . to match newlines.
           ;; Capture everything from SEARCH-PATTERN up to the next empty line.
           "rg -iN --color=always -B 3 -U '~a(.|\n)*?\n\n'"
           search-pattern))
         (cmd (format #f "~a | ~a | ~a" cmd1 cmd2 cmd3))
         (cmd-result-struct (exec cmd #:verbose verbose #:return-plist #t))
         (retcode (plist-get cmd-result-struct #:retcode))]
    (if (zero? retcode)
        (let* [(results (plist-get cmd-result-struct #:results))]
          (unless (null? results)
            ;; file in magenta
            (format #t "~a\n" (colorize-string file (color MAGENTA)))
            (map (partial format #t "~a\n") results)))
        ;; An error is returned by rg if nothing it passed to it. We need to
        ;; ignore the error. This can be achieved as well by defining cmd as:
        ;;     "~a | ~a | ~a || :"
        ;; where ':' is meant to be noop (no operation)
        ;; (error-command-failed m)
        )))
(testsymb 'search-file-ripgrep)

(def* (search-file-awk search-pattern file #:key (verbose #f))
  "
(search-file-awk \"search-pattern\"
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
/^[[:space:]]*$/ { print; next }
{ printf \"\\033[32m%d\\033[0m:  %s\\n\", NR, $0 }
' '~a'" file))

         ;; Filter blocks containing the search-pattern
         (cmd2 "sed 's/^\\s*$//'")

         ;; Color the search-pattern
         (cmd3
          (format
           #f
           ;; -v var=val		--assign=var=val
           ;; The variable assignment feature is most useful for assigning to
           ;; variables such as ‘RS’, ‘OFS’, and ‘ORS’,
           "awk -v RS='' 'BEGIN {IGNORECASE=1} /~a/ {gsub(/~a/, \"\\033[1;31m&\\033[0m\"); print $0 \"\\n\"}'"
           search-pattern search-pattern))
         (cmd (format #f "~a | ~a | ~a" cmd1 cmd2 cmd3))
         (cmd-result-struct (exec cmd #:verbose verbose #:return-plist #t))
         (retcode (plist-get cmd-result-struct #:retcode))]
    (if (zero? retcode)
        (let* [(results (plist-get cmd-result-struct #:results))]
          (unless (null? results)
            (format #t "~a\n" (colorize-string file (color MAGENTA))) ; file in magenta
            (map (partial format #t "~a\n") results)))
        (error (format #f "~a retcode: ~a\n" f retcode)))))

(def*-public (search-notes #:key (trace #f) #:rest args)
  "Usage:
(search-notes (list \"<ignored>\"
  \"-v\" \"rest \"
  \"/home/bost/org-roam/guix-guile-nix/guile.scrbl\"
  \"/home/bost/org-roam/guix-guile-nix/guile_scripting.scrbl\"))

-v/--verbose prints the sed/awk pipeline before it's executed.
"
  (when trace
    (format #t "~a trace   : ~a\n" f trace)
    (format #t "~a args    : ~a\n" f args))
  (let* [(arg-lst (car args))
         (option-spec '((verbose (single-char #\v) (value #f))))
         ;; `getopt-long' expects ARG-LST's car to be the program name, as
         ;; `command-line' would produce it -- it's skipped, not parsed.
         (options (getopt-long arg-lst option-spec))
         (verbose (option-ref options 'verbose #f))
         (positional-args (option-ref options '() '()))
         (search-pattern (car positional-args))
         (files (cdr positional-args))
         ]
    (when trace
      (format #t "arg-lst: '~a'\n" arg-lst)
      (format #t "option-spec: '~a'\n" option-spec)
      (format #t "options: '~a'\n" options)
      (format #t "verbose: '~a'\n" verbose)
      (format #t "positional-args: '~a'\n" positional-args)
      (format #t "search-pattern: '~a'\n" search-pattern)
      (format #t "files: '~a'\n" files)
      )
    (map (cut search-file-awk search-pattern <> #:verbose verbose)
         files)
    ))

(define-public main search-notes)

(testsymb 'main)

(module-evaluated)
