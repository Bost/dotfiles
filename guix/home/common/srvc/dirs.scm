(define-module (srvc dirs)
  ;; See service-file -> with-imported-modules
  #:use-module (utils)
  #:use-module (fs-utils)

  #:use-module (gnu services)
  ;; program-file local-file
  #:use-module (guix gexp)
  ;; simple-service
  #:use-module (gnu home services)

  ;; Enables evaluating (serialize-string "field-name" "val") from the REPL.
  ;; Not strictly needed. Can be imported by: ,use (gnu home services utils)
  #:use-module (gnu home services utils)

  ;; take remove delete-duplicates append-map etc.
  #:use-module (srfi srfi-1)
  ;; $fish-foreign-env
  ;; #:use-module (gnu packages shells)
  ;; pretty-print
  #:use-module (ice-9 pretty-print)

  #:use-module (gnu services configuration)
  #:use-module (gnu home services utils)
  ;; ,use (gnu home services utils)

  #:export (
            dirs-service-type
            dirs-configuration
            ))

(define m (module-name-for-logging))

;;;
;;; user directories.
;;;

;; (define dev-configuration-files-directory "dev")

;; (define (dev-configuration-files files)
;;   "Add dev/ prefix to each file-path in FILES."
;;   (map (match-lambda
;;          ((file-path . rest)
;;           (cons (string-append dev-configuration-files-directory "/" file-path)
;;                 rest)))
;;        files))

;; (define dev-configuration-files-service-type
;;   (service-type (name 'home-dev-configuration)
;;                 (extensions
;;                  (list (service-extension home-files-service-type
;;                                           dev-configuration-files)))
;;                 (compose concatenate)
;;                 (extend append)
;;                 (default-value '())
;;                 (description "Files that will be put in
;; @file{~/.guix-home/files/dev}, and further processed during activation.")))


(define (serialize-string field-name val)
  ;; The path has to be quoted
  (format #f "MY_~a_DIR=\"~a\"\n"
          (object->snake-case-string field-name 'upper) val))

(define-configuration dirs-configuration
  (dev (string "$HOME/dev") "Default development directory.")
  (der (string "$HOME/der") "Development directory for Racket project.")
  (dec (string "$HOME/dec") "Development directory for Clojure(Script) projects."))
(testsymb 'dirs-configuration)

;; (define (dirs-files-service config)
;;   `(("user-dirs.conf"
;;      ,(mixed-text-file
;;        "user-dirs.conf"
;;        "enabled=False\n"))
;;     ("user-dirs.dirs"
;;      ,(mixed-text-file
;;        "user-dirs.dirs"
;;       (serialize-configuration
;;        config
;;        dirs-configuration-fields)))))

(define (dirs-activation-service config)
  (let ((dirs (map (lambda (field)
                     ((configuration-field-getter field) config))
                   dirs-configuration-fields)))
    #~(let ((ensure-dir
             (lambda (path)
               ((@ (guix build utils) mkdir-p)
                ((@ (ice-9 string-fun) string-replace-substring)
                 path "$HOME" (getenv "HOME"))))))
        (display "Creating user directories...")
        (map ensure-dir '#$dirs)
        (display " done\n"))))
(testsymb 'dirs-activation-service)

(define (last-extension-or-cfg config extensions)
  "Picks configuration value from last provided extension.  If there
are no extensions use configuration instead."
  (or (and (not (null? extensions)) (last extensions)) config))
(testsymb 'last-extension-or-cfg)

(define dirs-service-type
  (service-type (name 'dirs)
                (extensions
                 (list
                  #;
                  (service-extension
                   dev-configuration-files-service-type
                   dirs-files-service)
                  (service-extension
                   ;; this is the services which I want to extend
                   home-activation-service-type
                   ;; this returns a list of objects. These objects extend the
                   ;; service specified by the 1st parameter of the
                   ;; service-extension
                   dirs-activation-service)))
                (default-value (dirs-configuration))
                (compose identity)
                (extend last-extension-or-cfg)
                (description "Configure user directories. To disable a
directory, point it to the $HOME.")))
(testsymb 'dirs-service-type)

(define (generate-dirs-documentation)
  (generate-documentation
   `((dirs-configuration
     ,dirs-configuration-fields))
   'dirs-configuration))
(testsymb 'generate-dirs-documentation)

;; (format #t "~a module evaluated\n" m)
