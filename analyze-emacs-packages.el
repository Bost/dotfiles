#!/home/bost/.guix-profile/bin/emacs --script

;; Run by:
;;    ./analyze-emacs-packages.el && ./create-elisp-package-definition.scm
;; See also:
;;    guix import elpa --archive=melpa <package-name-as-on-melpa>

;; for `-compose' and `-partial'
(require 'dash)
(require 'f)

(setq
 pkgs
 '(
   ;; (
   ;;  "<git-repo-without-.git>"
   ;;  "<version-number>"
   ;;  "<path-to-el-file>"
   ;;  )
   ))

(defun lint-get-forms (filename)
  "Read FILENAME and return a list of its Lisp forms."
  (with-temp-buffer
    (let ((buf (current-buffer))
          forms)
      (insert-file-contents filename)
      (while (ignore-errors (push (read buf) forms)))
      (nreverse forms))))

(pcase-defmacro sexp (kw) `(pred (equal ,kw)))
;; (sexp--pcase-macroexpander 'provide) ;; => (pred (equal provide))

(setq inputs-to-remove
      (mapcar (-compose
               (-partial #'concat "emacs-")
               #'symbol-name)
              ;; color, dash are needed
              ;; posframe is NOT part of Emacs, despite the comment
              '(
                advice
                ansi-color
                auth-source
                avoid
                calc-ext
                cl
                cl-extra
                cl-lib
                comint
                compile
                cus-face
                custom
                derived
                dired
                dom
                easy-mmode
                edmacro
                eieio
                eieio-base
                epg
                erc
                ert
                eshell
                find-func
                flymake
                flyspell
                font-lock
                format-spec
                goto-addr
                help-fns
                help-mode
                hierarchy
                hl-line
                ido
                image
                image-dired
                imenu
                ispell
                iswitchb ;; deprecated
                json
                latex
                let-alist
                lv
                magit
                map
                mode-local
                nxml-mode
                ob
                ob-core
                org
                org-capture
                package
                pcase
                pcomplete
                pdf-view  ;; in pdf-tools
                pp
                python
                regexp-opt
                reveal
                ring
                rx
                seq
                server
                sha1
                shell
                shr
                sql
                sqlite
                speedbar
                subr-x
                thingatpt
                tramp
                transient
                url
                url-queue
                url-parse
                url-http
                vc-hooks
                view
                warnings
                wid-edit
                xml
                xref
                )))

(defun find-sexps (kw lst)
  "Remove nil elements"
  (funcall
   (-compose
    ;; (-partial #'mapcar (-compose (-partial #'message "%s")))
    (-partial #'remove nil)
    (-partial #'mapcar
              (-compose
               (-partial
                (lambda (kw lst)
                  (pcase lst
                    (`(,(sexp kw) ,pkg)
;;; TODO find out what is the type of PKG? It's not a string neither symbol.
                     (concat "emacs-" (substring (format "%s" pkg) 1)))))
                kw))))
   lst))

(defun analyze (pkg)
  (funcall
   (-compose
    (lambda (lst)
      (list
       (append (find-sexps 'provide-theme lst) (find-sexps 'provide lst))
       ;; (find-sexps 'provide lst)
       ;; emacs-dash is needed
       (find-sexps 'require lst)))
    #'reverse
    #'lint-get-forms
    #'caddr)
   pkg))

(funcall
 (-compose
  (lambda (_)
    (message "%s" "Now run ./create-elisp-package-definition.scm"))
  (lambda (s)
    (f-write-text (format "%s" (pp
                                `(define pkgs-analyzed
                                         ',s)))
                  'utf-8 (format "%s/analyzed.scm" (getenv "dotf"))))
  ;; (-partial #'mapcar #'print)
  (-partial #'mapcar
            (lambda (pkg)
              "(git-url, version, elisp-file). E.g.:
   (
    \"https://github.com/osv/web-completion-data\"
    \"0.2\"
    \"/home/bost/.local/share/spacemacs/elpa/28.2/develop/web-completion-data-20160318.848/web-completion-data.el\"
    ) "

              (let ((r (analyze pkg)))
                (list (caar r)    ; package name - string
                      (car pkg)   ; repo url - string
                      (cadr pkg)  ; version
                      (let* ((input-syms     (mapcar #'intern (cadr r)))
                             (input-syms-rem (mapcar #'intern inputs-to-remove))
                             (inputs (cl-set-difference input-syms input-syms-rem)))
                        ;; (message "r: %s" r)
                        ;; (message "input-syms: %s" input-syms)
                        `(propagated-inputs
                          (list ,@inputs)))    ; required packages - list
                      (caddr pkg) ; file
                      )))))
 ;; (list (car pkgs) (cadr pkgs))
 pkgs)
