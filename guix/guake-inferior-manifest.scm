;; set profile "$HOME/.guix-extra-profiles/guake"
;; mkdir --parents "$profile"
;; guix package --profile="$profile" --manifest=guix/guake-inferior-manifest.scm
(use-modules (guix profiles)
             (guix channels)
             (guix inferior)
             (srfi srfi-1))

(define guake-inferior
  (inferior-for-channels
   (list
    (channel
     (name 'guix)
     (url "https://codeberg.org/guix/guix.git")
     (commit
      "396d955f0631edad3a972345cff2797cce0f4a63" ; bad
      ;; "9da01b88824c24b2bc453159ff18810fa978d50b" ; good
      ;; "3765eb786c0ef2ac85ab36f0da37e64569a86be6" ; good
      ;; "5ea7ac82a4bbfe98f4c74f80005fba4e2b3f3b95" ; good
      ;; "112303be80c2d20a963aa2311d5ab2af7135f585" ; build failed for: python-brotlicffi-1.0.9.2.drv
      ;; "a375202ad3443e4c3fb492160e32aa6abcc965bd" ; good
      )
     (introduction
      (make-channel-introduction
       "9edb3f66fd807b096b48283debdcddccfea34bad"
       (openpgp-fingerprint
        "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
    (channel
     (name 'guix-guake)
     (url
      "https://codeberg.org/Bost/guix-guake.git"
      ;; "file:///home/bost/dev/guix-guake"
      )))))

(packages->manifest
 (list
  (first (lookup-inferior-packages guake-inferior "guake"))))
