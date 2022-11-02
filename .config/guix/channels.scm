(
 ;; identity
 reverse
 (cons*

  ;; the flat channel doesn't compile at the moment
  #;
  (channel
   (name 'flat)
   (url
    ;; my local copy
    (string-append "file://" (getenv "HOME") "/dev/flatwatson")
    #;"https://github.com/flatwhatson/guix-channel.git")
   (introduction
    (make-channel-introduction
     "33f86a4b48205c0dc19d7c036c85393f0766f806"
     (openpgp-fingerprint
      "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))

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
   ;; pick one branch if not on master
   #;(branch "spacemacs" #;"racket"))

  ;; provides clojure, babashka, postgres 13.3, openjdk18 etc.
  (channel
   (name 'bost)
   (url
    #;"https://github.com/Bost/guix-packages"
    (string-append "file://" (getenv "HOME") "/dev/guix-packages")))

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
