(define-module (services starship-dotfiles)
  #:use-module (fs-utils)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages shellutils)
  #:use-module (guix gexp))

(define (dotfiles-starship-file name)
  (local-file (user-dotf "/.config/" name)
              #:recursive? #t))

(define-public (starship-dotfiles-service)
  (simple-service
   'starship-dotfiles
   home-xdg-configuration-files-service-type
   `(("starship.toml"
      ,(dotfiles-starship-file "starship.toml"))
     )))

(define-public (starship-package-service)
  (simple-service
   'starship-package
   home-profile-service-type
   (list starship)))

(define-public (starship-services)
  (list
   (starship-dotfiles-service)
   (starship-package-service)))
