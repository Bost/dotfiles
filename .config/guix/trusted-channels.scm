(list
 (channel
  (name 'guix-ai-cloud)
  (url
   ;; "https://codeberg.org/daym/guix-ai-cloud"
   "https://codeberg.org/Bost/guix-ai-cloud"
   )
  (branch "master")
  ;; (commit "653504e6551198c9b2b998c143d7cf2675b22547")
  (introduction
   (make-channel-introduction
    ;; "ba6015f3120e56a18eeb31ee31cbd0efc25dbb94" ; Danny's 1st commit
    ;; "0051f7605d10c563c2092a26c4a45e4320d2a04f" ; Authorize Rostislav Svoboda's key
    "05ca2453b4650f7a82b28234f9e512a4ce80d4a4" ; guix-channel: Add channel-introduction for nonguix
    (openpgp-fingerprint
     "76CE C6B1 7274 B465 C02D B3D9 E71A 3554 2C30 BAA5"))))

 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (branch "master")
  (commit "653504e6551198c9b2b998c143d7cf2675b22547")
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))

 (channel
  (name 'bost)
  (url "https://codeberg.org/Bost/guix-packages")
  (branch "master")
  (commit "b2c3b690122e0afec40ade444611df2f515758ca")
  (introduction
   (make-channel-introduction
    "d92cc7a959fbde6ff4ac202e43069ddf37be233c"
    (openpgp-fingerprint
     "A2FE D89D 9A10 000E 5BF6  3B37 612D 3636 8877 DC81"))))

 (channel
  (name 'guix)
  (url "https://git.guix.gnu.org/guix.git")
  (branch "master")
  (commit "bdc339838a450b64413dc7a4d9fa78cdb30062f7")
  (introduction
   (make-channel-introduction
    "9edb3f66fd807b096b48283debdcddccfea34bad"
    (openpgp-fingerprint
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
