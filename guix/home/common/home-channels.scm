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
                                  guix-science-commit
                                  guix-past-commit
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

   ;; When firefox substitutes are not available in the nonguix channel. Fetch
   ;; them from the guix-sciene
   (channel-guix-science #:commit guix-science-commit)

   (channel-guix-android #:commit guix-android-commit)

   ;; The `guix-past' channel is not needed directly, however it is required by
   ;; the `games' channel, which, without this pinning would pull from the
   ;; latest channel version
   (channel-guix-past #:commit guix-past-commit)

   ;; whereiseveryone
   ;; (channel-guixrus #:commit guixrus-commit)

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
                     guix-science-commit
                     guix-android-commit
                     ;; guixrus-commit
                     hask-clj-commit
                     games-commit
                     bost-commit
                     guix-commit
                     nonguix-commit
                     guix-past-commit
                     #:allow-other-keys
                     )
  ((comp
    (lambda (lst)
      (if (or (host-edge?) (host-ecke?) (host-geek?))
          (append
           (list (channel-nonguix #:commit nonguix-commit))
           (home-channels-edge-ecke
            #:guix-science-commit  guix-science-commit
            #:guix-past-commit     guix-past-commit
            #:guix-android-commit  guix-android-commit
            ;; #:guixrus-commit       guixrus-commit
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

 ;; ;; 30 jan. 2026 19:23:25
 ;; #:nonguix-commit      "216cf961b56a8b2679239ba102089ab94c259e89"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guixrus-commit      "9f5a0a357ffafbe9b31fcdc3916dfee57baf5859"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "acc252d2f7fed939ace5a5a98d7750197696dac3"
 ;; #:bost-commit         "748226b0987ae6eca76f8a5d4713451f641cce9e"
 ;; #:guix-commit         "a6dddbb062ebc9ed20a51dbec0f1f2e9b6dba77c"
 ;; #:guix-past-commit    "be7997692e81a89817c7fa2d6e36aee71c8e6916"

 ;; #:nonguix-commit      "216cf961b56a8b2679239ba102089ab94c259e89"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guixrus-commit      "9f5a0a357ffafbe9b31fcdc3916dfee57baf5859"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "acc252d2f7fed939ace5a5a98d7750197696dac3"
 ;; #:bost-commit         "aa676169857506d61db08cbaed3a93c43c1c2078"
 ;; #:guix-commit         "a6dddbb062ebc9ed20a51dbec0f1f2e9b6dba77c"
 ;; #:guix-past-commit    "be7997692e81a89817c7fa2d6e36aee71c8e6916"

 ;; ;; 05 feb. 2026 10:41:44
 ;; #:nonguix-commit      "6c0ea215e0bd089bf3b2097e5c59dd726fbbe304"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guixrus-commit      "9f5a0a357ffafbe9b31fcdc3916dfee57baf5859"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "acc252d2f7fed939ace5a5a98d7750197696dac3"
 ;; #:bost-commit         "627b59db1e8155d51ac5a1d8cfcc2d81256730f9"
 ;; #:guix-commit         "2c710762f5c80b1a151b57c6f369dfa1812d1f97"
 ;; #:guix-past-commit    "be7997692e81a89817c7fa2d6e36aee71c8e6916"

 ;; 07 feb. 2026 12:15:47
 ;; #:nonguix-commit      "6c0ea215e0bd089bf3b2097e5c59dd726fbbe304"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guixrus-commit      "9f5a0a357ffafbe9b31fcdc3916dfee57baf5859"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "acc252d2f7fed939ace5a5a98d7750197696dac3"
 ;; #:bost-commit         "627b59db1e8155d51ac5a1d8cfcc2d81256730f9"
 ;; #:guix-commit         "ec5fb6678f8268437b1940f7ed2f2b72d62ab4e0"
 ;; #:guix-past-commit    "be7997692e81a89817c7fa2d6e36aee71c8e6916"

 ;; 18 feb. 2026 12:50:26
 ;; #:nonguix-commit      "1980960f932063f42f97ad3be4b020f68d24e62b"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guixrus-commit      "9f5a0a357ffafbe9b31fcdc3916dfee57baf5859"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit        "fcf8e459d8a4fc2800d4a1af14f1c3c6c714e419"
 ;; #:bost-commit         "627b59db1e8155d51ac5a1d8cfcc2d81256730f9"
 ;; #:guix-commit         "0b6b8a1e88ada49cb3303be8d4eefe95dca704dd"
 ;; #:guix-past-commit    "e5f020d2ea444861379120655e10da7824ad2606"

 ;; 26 feb. 2026 12:46:06
 ;; #:nonguix-commit       "da4e72efef62d48dbc2eb089c36972ff55fe6acd"
 ;; #:guix-android-commit  "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:hask-clj-commit      "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit         "fcf8e459d8a4fc2800d4a1af14f1c3c6c714e419"
 ;; #:bost-commit          "627b59db1e8155d51ac5a1d8cfcc2d81256730f9"
 ;; ;; #:guix-commit          "2f95cb9f470c7179ae219ebae969cf5f1c041994" ; See also https://codeberg.org/guix/guix/issues/6714
 ;; #:guix-commit          "c07690f0d673d8415066160617455313afa1d544" ; good
 ;; #:guix-past-commit     "bc95e89da6045cec1797673ba3283122a11e6ee7"

 ;; 04. mar. 2026 17:26:51
 ;; #:nonguix-commit       "da4e72efef62d48dbc2eb089c36972ff55fe6acd"
 ;; #:guix-android-commit  "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:hask-clj-commit      "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit         "fcf8e459d8a4fc2800d4a1af14f1c3c6c714e419"
 ;; #:bost-commit          "84999be7778fc2f08e09b056a3aa09afbd9d0c2c"
 ;; #:guix-commit          "c07690f0d673d8415066160617455313afa1d544"
 ;; #:guix-past-commit     "bc95e89da6045cec1797673ba3283122a11e6ee7"

 ;; 04 mar. 2026 20:25:53
 ;; #:nonguix-commit       "816b5c6f45ed56cd81e2a6338ed5b710e4e66e98"
 ;; #:guix-android-commit  "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:hask-clj-commit      "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit         "348775cb228def51bba4f4502e68a078f492b72f"
 ;; #:bost-commit          "84999be7778fc2f08e09b056a3aa09afbd9d0c2c"
 ;; #:guix-commit          "a375202ad3443e4c3fb492160e32aa6abcc965bd"
 ;; #:guix-past-commit     "bc95e89da6045cec1797673ba3283122a11e6ee7"

 ;; 06 mar. 2026 17:01:23
 ;; #:nonguix-commit       "816b5c6f45ed56cd81e2a6338ed5b710e4e66e98"
 ;; #:guix-android-commit  "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:hask-clj-commit      "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:games-commit         "348775cb228def51bba4f4502e68a078f492b72f"
 ;; #:bost-commit          "70d2459d1123e5b5a0d1b12979c216e652ef96c6"
 ;; #:guix-commit          "a375202ad3443e4c3fb492160e32aa6abcc965bd"
 ;; #:guix-past-commit     "f5d1cdc58ea810c9d1f1858289b01e9fdc3e4b8f"

 ;; 14 mar. 2026 22:41:34
 ;; #:games-commit         "348775cb228def51bba4f4502e68a078f492b72f"
 ;; #:bost-commit          "9d4396f30a55558b48655a25bc6d9915ea4e13b0"
 ;; #:guix-android-commit  "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-commit          "a375202ad3443e4c3fb492160e32aa6abcc965bd"
 ;; #:guix-past-commit     "f5d1cdc58ea810c9d1f1858289b01e9fdc3e4b8f"
 ;; #:hask-clj-commit      "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:nonguix-commit       "7d14e819fc7b2681240023b9e4c001fc3f5cba04"

 ;; 27 mar. 2026 15:51:03
 ;; #:bost-commit         "4bc752e30201ea9415502909dc3a79749bf4e8a1"
 ;; #:games-commit        "348775cb228def51bba4f4502e68a078f492b72f"
 ;; #:guix-android-commit "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-commit         "29cc182c3571df423ec298044f02f07106cb91d9"
 ;; #:guix-past-commit    "d0e01528abcf07a40981303b3e4a54edd7ffd501"
 ;; #:hask-clj-commit     "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:nonguix-commit      "b8648bf188df213455983e7eaab9f6c6101ca0c2"

 ;; 12 apr. 2026 13:30:56
 ;; #:bost-commit          "29e431dd2d92175ac54a4ee4e93bacd25a9f5a63"
 ;; #:games-commit         "348775cb228def51bba4f4502e68a078f492b72f"
 ;; #:guix-android-commit  "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 ;; #:guix-commit          "f1cacebca6d7f9afc7fc81fb18ef6655b12d7f3c"
 ;; #:guix-past-commit     "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 ;; #:guix-science-commit  "937ad09462d85797a61001ab2c0d664320420b92"
 ;; #:hask-clj-commit      "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 ;; #:nonguix-commit       "c4541fdb0b472664dafe5d7b1ec2e51e4ef7b772"

 ;; 12 apr. 2026 14:14:43
 #:bost-commit          "84999be7778fc2f08e09b056a3aa09afbd9d0c2c"
 #:games-commit         "348775cb228def51bba4f4502e68a078f492b72f"
 #:guix-android-commit  "e5f52bd57275e404db74bf03b541bb62f7d73d58"
 #:guix-commit          "a375202ad3443e4c3fb492160e32aa6abcc965bd"
 #:guix-past-commit     "82f634f37c1d7b6d4051aa126f7b835c2419e58b"
 #:guix-science-commit  "937ad09462d85797a61001ab2c0d664320420b92"
 #:hask-clj-commit      "a8b30a606f91caabec3cc8dc4b1255a69836554e"
 #:nonguix-commit       "816b5c6f45ed56cd81e2a6338ed5b710e4e66e98"
 )
