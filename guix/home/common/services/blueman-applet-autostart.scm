(define-module (services blueman-applet-autostart)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages networking)
  #:use-module (guix gexp))

(define (blueman-applet-autostart-service)
  (simple-service 'blueman-applet-autostart
                  home-xdg-configuration-files-service-type
                  `(("autostart/blueman-applet.desktop"
                     ,(plain-file
                       "blueman-applet.desktop"
                       "[Desktop Entry]
Type=Application
Name=Blueman Applet
Exec=blueman-applet
Terminal=false
X-GNOME-Autostart-enabled=true
")))))

(define-public (blueman-package-service)
  (simple-service
   'blueman-package
   home-profile-service-type
   (list blueman)))

(define-public (blueman-services)
  (list
   (blueman-applet-autostart-service)
   (blueman-package-service)))
