;; `guix pull` places a union of this file with %default-channels to
;; `~/.config/guix/current/manifest`. See
;; guile -c '(use-modules (guix channels)) (format #t "~a\n" %default-channels)'

;; This module is loaded via
;;     --load-path=/home/bost/dev/dotfiles/guix/home/common
;; so the module-name is not (home common home-channels)
(define-module (home-channels)
  #:use-module (gnu services)           ; simple-service
  #:use-module (gnu home services guix) ; home-channels-service-type

  #:use-module (dotf config channels channel-defs)
  #:use-module (dotf utils)
  #:use-module (dotf memo)
  )

(define m (module-name-for-logging))
(evaluating-module)

(define-public (home-channels-services)
  (list
   ;; (simple-service 'guixrus-service  home-channels-service-type (list (channel-guixrus)))
   (simple-service 'hask-clj-service home-channels-service-type (list (channel-hask-clj)))
   (simple-service 'bost-service     home-channels-service-type (list (channel-bost))))
  )

(define* (home-channels-edge-ecke #:key
                                  guix-android-commit
                                  guixrus-commit
                                  hask-clj-commit
                                  games-commit
                                  bost-commit
                                  )
  (list
   ;; dwl window manager for Wayland with dynamic configuration in Guile.
   ;; dwl-guile is a fork of the dwl Wayland Compositor (which is a
   ;; port of dwm - dynamic window manager for X).
   ;; (channel-home-service-dwl-guile)

   (channel-guix-android #:commit guix-android-commit)
   ;; (channel-guix-past) ;; pulled-in via channel-games; not needed directly.

   ;; whereiseveryone
   (channel-guixrus #:commit guixrus-commit)

   ;; (channel-hask-clj #:commit hask-clj-commit)
   (channel-hask-clj #:commit hask-clj-commit)

   ;; For factorio, pulls-in nonguix guix-past
   (channel-games #:commit games-commit)

   ;; (channel-home-service-dwl-guile)
   ;; (channel-flat)
   ;; (channel-rde)

   ;; pulls-in: guix nonguix guix-rust-past-crates
   (channel-bost #:commit bost-commit)))

(def* (home-channels #:key
                        guix-android-commit
                        guixrus-commit
                        hask-clj-commit
                        games-commit
                        bost-commit
                        guix-commit
                        nonguix-commit
                        #:allow-other-keys
                        )
  ((comp
    (lambda (lst)
      (if (or (host-edge?) (host-ecke?) (host-geek?))
          (append
           (list (channel-nonguix #:commit nonguix-commit))
           (home-channels-edge-ecke
            #:guix-android-commit  guix-android-commit
            #:guixrus-commit       guixrus-commit
            #:hask-clj-commit      hask-clj-commit
            #:games-commit         games-commit
            #:bost-commit          bost-commit
            ) lst)
          lst)))
   (list (channel-guix #:commit guix-commit))))

(module-evaluated)

(home-channels
 ;; ;; 12 jan. 2026 12:30:03
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; ;; #:guixrus-commit from file:///home/bost/dev/guixrus
 ;; #:guixrus-commit      "9f5a0a357ffafbe9b31fcdc3916dfee57baf5859"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "acc252d2f7fed939ace5a5a98d7750197696dac3"
 ;; #:bost-commit         "09700d780a7f706c910221d5d1de9378dc56ab12"
 ;; #:guix-commit         "62c28bc6d8c54800b16341d1c93a1d7833fc3328"
 ;; #:nonguix-commit      "3ceef556e1ce8e52180b959a942b16000ed4e61c"

 ;; ;; 30 jan. 2026 01:29:12
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; ;; #:guixrus-commit from file:///home/bost/dev/guixrus
 ;; #:guixrus-commit      "9f5a0a357ffafbe9b31fcdc3916dfee57baf5859"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "acc252d2f7fed939ace5a5a98d7750197696dac3"
 ;; #:bost-commit         "0d2c6cf3cdcaecaf06c63c7d111ddf615ecc240d"
 ;; #:guix-commit         "f2a0cf7633b65f906a437180f54517e7d5ac23d1"
 ;; #:nonguix-commit      "216cf961b56a8b2679239ba102089ab94c259e89"

 ;; ;; 30 jav. 2026 14:36:11
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; ;; #:guixrus-commit from file:///home/bost/dev/guixrus
 ;; #:guixrus-commit      "9f5a0a357ffafbe9b31fcdc3916dfee57baf5859"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "acc252d2f7fed939ace5a5a98d7750197696dac3"
 ;; #:bost-commit         "133c20de340312a76696678b7cde0ae35044f6f5"
 ;; #:guix-past-commit    "be7997692e81a89817c7fa2d6e36aee71c8e6916"
 ;; #:guix-commit         "d25404a99832a059efcb68a2c97352c15924dd83"
 ;; #:nonguix-commit      "216cf961b56a8b2679239ba102089ab94c259e89"

 ;; ;; 30 jan. 2026 15:27:43
 ;; #:nonguix-commit      "216cf961b56a8b2679239ba102089ab94c259e89"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guixrus-commit      "9f5a0a357ffafbe9b31fcdc3916dfee57baf5859"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "acc252d2f7fed939ace5a5a98d7750197696dac3"
 ;; #:bost-commit         "748226b0987ae6eca76f8a5d4713451f641cce9e"
 ;; #:guix-commit         "d25404a99832a059efcb68a2c97352c15924dd83"
 ;; #:guix-past-commit    "be7997692e81a89817c7fa2d6e36aee71c8e6916"

 ;; 30 jan. 2026 19:23:25
 #:nonguix-commit      "216cf961b56a8b2679239ba102089ab94c259e89"
 #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 #:guixrus-commit      "9f5a0a357ffafbe9b31fcdc3916dfee57baf5859"
 #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 #:games-commit        "acc252d2f7fed939ace5a5a98d7750197696dac3"
 #:bost-commit         "748226b0987ae6eca76f8a5d4713451f641cce9e"
 #:guix-commit         "a6dddbb062ebc9ed20a51dbec0f1f2e9b6dba77c"
 #:guix-past-commit    "be7997692e81a89817c7fa2d6e36aee71c8e6916"
 )
