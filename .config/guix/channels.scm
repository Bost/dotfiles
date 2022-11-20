;; `guix pull` places a union of this file with %default-channels to
;; `~/.config/guix/current`. See
;; $ guix repl
;; scheme@(guix-user)> (use-modules (guix channels))
;; scheme@(guix-user)> %default-channels
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

 ;; provides clojure, babashka, postgres 13.3, openjdk18 etc.
 (channel
  (name 'bost)
  (url
   #;"https://github.com/Bost/guix-packages"
   (string-append "file://" (getenv "HOME") "/dev/guix-packages")))

 ;; The guix-gaming-games channel is added programmatically from the
 ;; home-configuration.scm

 %default-channels)
