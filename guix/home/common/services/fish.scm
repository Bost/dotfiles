(define-module (services fish)
  #:use-module (fs-utils)
  #:use-module (dotf utils)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1) ; list-processing procedures
  #:export (
            home-my-fish-service-type
            home-my-fish-configuration
            home-my-fish-extension
            ))

(define m (module-name-for-logging))
(evaluating-module)

(define-syntax get-from
  (lambda (stx)
    (syntax-case stx ()
      ((_ record-type record field)
       (let* ((record-type-name (syntax->datum #'record-type))
              (field-name       (syntax->datum #'field))
              (accessor-name
               (string->symbol
                (string-append (symbol->string record-type-name)
                               "-"
                               (symbol->string field-name)))))
         #`(#,(datum->syntax #'record-type accessor-name) record))))))
(testsymb 'get-from)

(define-syntax-rule (config-ref config field)
  (get-from home-my-fish-configuration config field))

(define-syntax-rule (extension-ref extension field)
  (get-from home-my-fish-extension extension field))


(define (dotfiles-fish-file name)
  (local-file (user-dotf "/.config/fish/" name)
              #:recursive? #t))

;; Prefer `define-record-type*' over `define-configuration'
(define-record-type* <home-my-fish-configuration>
  home-my-fish-configuration make-home-my-fish-configuration
  home-my-fish-configuration?
  (config-dot-fish
   home-my-fish-configuration-config-dot-fish
   (default
     (list
      (dotfiles-fish-file "config.fish"))))

  (environment-variables
   home-my-fish-configuration-environment-variables
   (default
     (list)))
  (aliases
   home-my-fish-configuration-aliases
   (default
     (list)))
  (abbreviations
   home-my-fish-configuration-abbreviations
   (default
     (list)))
  (xdg-configuration-files
   home-my-fish-configuration-xdg-configuration-files
   (default
     `(("fish/fish_plugins" ,(dotfiles-fish-file "fish_plugins"))
       ("fish/completions"  ,(dotfiles-fish-file "completions"))
       ("fish/functions"    ,(dotfiles-fish-file "functions"))))))
(testsymb 'home-my-fish-configuration)

(define (my-fish-service full-config)
  (home-fish-extension
    (config
     (config-ref full-config config-dot-fish))
    (environment-variables
     (config-ref full-config environment-variables))
    (aliases
     (config-ref full-config aliases))
    (abbreviations
     (config-ref full-config abbreviations))))
(testsymb 'my-fish-service)

(define (my-fish-xdg-configuration-files-service full-config)
  (home-my-fish-configuration-xdg-configuration-files full-config))
(testsymb 'my-fish-xdg-configuration-files-service)

;; Prefer `define-record-type*' over `define-configuration/no-serialization'
(define-record-type* <home-my-fish-extension>
  home-my-fish-extension make-home-my-fish-extension
  home-my-fish-extension?
  (fish-extension
   home-my-fish-extension-fish-extension
   (default (home-fish-extension)))
  (xdg-configuration-files
   home-my-fish-extension-xdg-configuration-files
   (default '())))
(testsymb 'home-my-fish-extension)

(define (home-my-fish-extensions original-config extension-configs)
  (home-my-fish-configuration
   (inherit original-config)
   (config-dot-fish
    (append (config-ref original-config config-dot-fish)
            (append-map
             (lambda (extension)
               (extension-ref extension config-dot-fish))
             extension-configs)))
   (environment-variables
    (append (config-ref original-config environment-variables)
            (append-map
             (lambda (extension)
               (extension-ref extension environment-variables))
             extension-configs)))
   (aliases
    (append (config-ref original-config aliases)
            (append-map
             (lambda (extension)
               (extension-ref extension aliases))
             extension-configs)))
   (abbreviations
    (append (config-ref original-config abbreviations)
            (append-map
             (lambda (extension)
               (extension-ref extension abbreviations))
             extension-configs)))
   (xdg-configuration-files
    (append (config-ref original-config xdg-configuration-files)
            (append-map
             (lambda (extension)
               (extension-ref extension xdg-configuration-files))
             extension-configs)))))
(testsymb 'home-my-fish-extensions)

(define home-my-fish-service-type
  (service-type (name 'home-my-fish)
                (extensions
                 (list (service-extension
                        home-fish-service-type
                        my-fish-service)
                       (service-extension
                        home-xdg-configuration-files-service-type
                        my-fish-xdg-configuration-files-service)))
                (compose concatenate)
                (extend home-my-fish-extensions)
                (default-value (home-my-fish-configuration))
                (description "\
Install fish through Guix Home and provision fish configuration files.")))

(module-evaluated)
