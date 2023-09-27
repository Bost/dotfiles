(list (channel
        (name 'games)
        (url "https://gitlab.com/guix-gaming-channels/games.git")
        (branch "master")
        (commit
          "e406d41a80ec97419190f7c30d2605264301c407")
        (introduction
          (make-channel-introduction
            "c23d64f1b8cc086659f8781b27ab6c7314c5cca5"
            (openpgp-fingerprint
              "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F"))))
      (channel
        (name 'guix-past)
        (url "https://gitlab.inria.fr/guix-hpc/guix-past")
        (branch "master")
        (commit
          "1e25b23faa6b1716deaf7e1782becb5da6855942")
        (introduction
          (make-channel-introduction
            "0c119db2ea86a389769f4d2b9c6f5c41c027e336"
            (openpgp-fingerprint
              "3CE4 6455 8A84 FDC6 9DB4  0CFB 090B 1199 3D9A EBB5"))))
      (channel
        (name 'bost)
        (url "file:///home/bost/dev/guix-packages")
        (branch "master")
        (commit
	  "6cbff072327122a364dc2a0532849acf69dbcd31"))
      (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (branch "master")
        (commit
	 ;; HEAD:
         ;; "15fdeaeb2d4df755811db92cc7c3c7cb19155dec"
	 ;; last-good:
         "4f35ff1275e05be31f5d41464ccf147e9dbfd016")
        #;(tag "last-good")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
        (name 'haskell-and-clojure)
        (url "file:///home/bost/dev/haskell-guix")
        (branch "master")
        (commit
          "a8b30a606f91caabec3cc8dc4b1255a69836554e"))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
          "bb184bd0a8f91beec3a00718759e96c7828853de")
        (introduction
          (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))
