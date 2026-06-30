(list (channel
       (name 'nonguix)
       (url "https://gitlab.com/nonguix/nonguix")
       (branch "master")
       (commit "d35a2f8f22023426ccf3598fa7079b09bb821e3e")
       (introduction
        (make-channel-introduction
         "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
         (openpgp-fingerprint
          "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
       (name 'bost)
       (url "https://codeberg.org/Bost//guix-packages")
       (branch "master")
       (introduction
        (make-channel-introduction
         "62a193df129b5a7a0da6af4f586b2b51d5b10629"
         (openpgp-fingerprint
          "A2FE D89D 9A10 000E 5BF6  3B37 612D 3636 8877 DC81"))))
      (channel
       (name 'guix)
       (url "https://git.guix.gnu.org/guix.git")
       (branch "master")
       (commit "f3ff036b6c4eba8a967d880cb6dda49b272edf98")
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
