(define-module (services flatpak)
  #:use-module (aurtzy home services package-management)
  #:use-module (utils)
  ;; #:use-module (settings)
  ;; #:use-module (memo)
  ;; #:use-module (fs-utils)
  #:use-module (gnu services)
  #:use-module (guix gexp)         ; program-file local-file
  #:use-module (gnu home services) ; simple-service
  #:use-module (srfi srfi-1)       ; list-processing procedures
  #:use-module (ice-9 pretty-print)
)

(define m (module-name-for-logging))
(evaluating-module)

;; (define-public (flatpak-service)
;;   ;; return a service of <type>, having <configuration> as its value
;;   (service home-flatpak-service-type
;;            (home-flatpak-configuration
;;             (remotes
;;              '((flathub
;;                 . "https://flathub.org/repo/flathub.flatpakrepo"))))))

;; ;; Extend the home-flatpak-profile-service-type with  '(...)
;; ;; and call the extension 'home-flatpak-browsers
;; (simple-service 'home-flatpak-browsers
;;                 home-flatpak-profile-service-type
;;                 '((flathub "org.mozilla.firefox")
;;                   (flathub "com.brave.Browser")))


(define-public (telegram-from-flatpak-service)
  (service home-flatpak-service-type
           (home-flatpak-configuration
            (remotes
             '((flathub-remote
                . "https://flathub.org/repo/flathub.flatpakrepo")))

            (profile
             '((flathub-remote "org.telegram.desktop")))
            )))
(testsymb 'telegram-from-flatpak-service)

#|
#<
  <service>
  type: #<service-type home-flatpak 7f451952fa40>
  value: #<
           <home-flatpak-configuration>
           flatpak: #<package flatpak@1.16.0 gnu/packages/package-management.scm:2199 7f450b0ef000>
           remotes: ((flathub-remote . "https://flathub.org/repo/flathub.flatpakrepo"))
           profile: ((flathub-remote "org.telegram.desktop"))
          >
 >
|#

(module-evaluated)
