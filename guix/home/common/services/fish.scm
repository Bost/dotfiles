(define-module (services fish)
  #:use-module (fs-utils)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages shells)
  #:use-module (guix gexp))

(define (dotfiles-fish-file name)
  (local-file (user-dotf "/.config/fish/" name)
              #:recursive? #t))

(define (fish-dotfiles-service)
  (simple-service
   'fish-dotfiles
   home-xdg-configuration-files-service-type
   `(("fish/config.fish"
      ,(dotfiles-fish-file "config.fish"))

     ("fish/fish_plugins"
      ,(dotfiles-fish-file "fish_plugins"))

     ("fish/completions"
      ,(dotfiles-fish-file "completions"))

     ("fish/conf.d"
      ,(dotfiles-fish-file "conf.d"))

     ("fish/functions"
      ,(dotfiles-fish-file "functions"))

     ;; Optional, but usually not recommended:
     ;; ("fish/fish_variables"
     ;;  ,(dotfiles-fish-file "fish_variables"))
     )))

(define (fish-package-service)
  (simple-service
   'fish-package
   home-profile-service-type
   (list fish)))

(define-public (fish-services)
  (list
   (fish-dotfiles-service)
   (fish-package-service)))
