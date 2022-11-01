(
 ;; identity
 reverse
 (cons*

  ;; See https://guix.gnu.org/manual/devel/en/html_node/Channels-with-Substitutes.html

  ;; provides a.o. firefox
  (channel
   (name 'nonguix)
   (url "https://gitlab.com/nonguix/nonguix")
   ;; Enable signature verification:
   (introduction
    (make-channel-introduction
     "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
     (openpgp-fingerprint
      "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))

  #;
  (channel
   (name 'my-guix)
   (url
    (string-append "file://" (getenv "HOME") "/dev/guix"))
   (branch "spacemacs"))

  #;
  (channel
   (name 'my-racket)
   (url
    (string-append "file://" (getenv "HOME") "/dev/guix"))
   (branch "racket"))

  ;; provides clojure, babashka, postgres 13.3, openjdk18 etc.
  (channel
   (name 'bost)
   (url
    #;"https://github.com/Bost/guix-packages"
    (string-append "file://" (getenv "HOME") "/dev/guix-packages")))

  #;
  (channel
   (name 'babashka)
   (url
    #;"https://github.com/Bost/guix-packages"
    (string-append "file://" (getenv "HOME") "/dev/babashka")))

  ;; https://raw.githubusercontent.com/wube/factorio-data/master/changelog.txt

  ;; guix package --load-path=./ --install=factorio
  ;; # --keep-failed doesn't keep the binary in the /gnu/store when the sha256
  ;; # is wrong
  #;
  (channel
   (name 'games)
   (url
    #;"https://gitlab.com/rostislav.svoboda/games"
    (string-append "file://" (getenv "HOME") "/dev/games")))

  %default-channels
  ))
