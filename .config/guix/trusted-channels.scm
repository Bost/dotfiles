;;; Trusted channels are identified by their `introduction'; fields such as
;;; `name' and `url' are ignored for trust matching. This also allows mirrors
;;; to be trusted without trusting each mirror URL separately.
;;; `name' and `url' are still required to construct a `channel' object.
;;;
;;; See guix/common/dotf/config/channels/channel-defs.scm
(list

 (channel
  (name 'guix-ai-cloud)
  (url "https://codeberg.org/Bost/guix-ai-cloud")
  (introduction
   (make-channel-introduction
    "0051f7605d10c563c2092a26c4a45e4320d2a04f"
    (openpgp-fingerprint
     "A2FE D89D 9A10 000E 5BF6  3B37 612D 3636 8877 DC81"))))

 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))

 (channel
  (name 'bstx)
  (url "https://codeberg.org/Bost/bstx")
  (introduction
   (make-channel-introduction
    "d92cc7a959fbde6ff4ac202e43069ddf37be233c"
    (openpgp-fingerprint
     "A2FE D89D 9A10 000E 5BF6  3B37 612D 3636 8877 DC81"))))

 (channel
  (name 'guix)
  (url "https://git.guix.gnu.org/guix.git")
  (introduction
   (make-channel-introduction
    "9edb3f66fd807b096b48283debdcddccfea34bad"
    (openpgp-fingerprint
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
