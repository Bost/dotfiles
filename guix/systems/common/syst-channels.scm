;; `guix pull` places a union of this file with %default-channels to
;; `~/.config/guix/current/manifest`. See
;; guile -c '(use-modules (guix channels)) (format #t "~a\n" %default-channels)'

;; This module is loaded via
;;     --load-path=/home/bost/dev/dotfiles/guix/systems/common
;; so the module-name is not (systems common home-channels)

(define-module (syst-channels)
  #:use-module (dotf config channels channel-defs)
  #:use-module (dotf utils)
  #:use-module (dotf memo)
  )

(define m (module-name-for-logging))
(evaluating-module)

(def* (syst-channels #:key
                     guix-science-commit
                     guix-commit
                     nonguix-commit
                     #:allow-other-keys
                     )
  ((comp
    (lambda (lst)
      (if (or (host-edge?) (host-ecke?) (host-geek?))
          (append
           (list
            (channel-guix-science #:commit guix-science-commit)
            (channel-nonguix #:commit nonguix-commit)
            ) lst)
          lst)))
   (list (channel-guix #:commit guix-commit))))

(module-evaluated)

(syst-channels
 ;; 15 feb. 2026 13:47:37
 ;; #:nonguix-commit "bbbc850e8a94833c9958625eb4c78a206fbacc1d"
 ;; #:guix-commit "7113a63ff4bdcf24fbc4bb92367ea6354dd33290"

 ;; 18 feb. 2026 18:07:07
 ;; #:nonguix-commit "1980960f932063f42f97ad3be4b020f68d24e62b"
 ;; #:guix-commit "0b6b8a1e88ada49cb3303be8d4eefe95dca704dd"

 ;; 26 feb. 2026 14:22:42
 ;; #:nonguix-commit "da4e72efef62d48dbc2eb089c36972ff55fe6acd"
 ;; #:guix-commit "c07690f0d673d8415066160617455313afa1d544"

 ;; 04 mar. 2026 20:53:35
 ;; #:nonguix-commit "816b5c6f45ed56cd81e2a6338ed5b710e4e66e98"
 ;; #:guix-commit "a375202ad3443e4c3fb492160e32aa6abcc965bd"

 ;; 15 mar. 2026 12:05:35
 ;; #:nonguix-commit "7d14e819fc7b2681240023b9e4c001fc3f5cba04"
 ;; #:guix-commit "a375202ad3443e4c3fb492160e32aa6abcc965bd"

 ;; 12 apr. 2026 01:53:12
 ;; #:guix-science-commit "937ad09462d85797a61001ab2c0d664320420b92"
 ;; #:nonguix-commit "c4541fdb0b472664dafe5d7b1ec2e51e4ef7b772"
 ;; #:guix-commit "f1cacebca6d7f9afc7fc81fb18ef6655b12d7f3c"

 ;; 16 avril 2026 16:31:45
 ;; #:guix-science-commit "f81cc0ac8a7c75e732ee7a1bf2cf27099f07f284"
 ;; #:nonguix-commit "5d8b56d6a07cdc1aa3022737c32ccb95f08aef08"
 ;; #:guix-commit "41ab1cb9e44aca2f77534d4d2fa4386b039f74f9"

 ;; 30 avril 2026 14:08:50
 ;; #:guix-science-commit "8ce03731a8eb84afec21953d5706aa72199f6649"
 ;; #:nonguix-commit      "a3f4e7bff779da4593a2922516064a8edaafa3e6"
 ;; #:guix-commit         "2dde6fc80f96cd8b1edef8f61637cc2adeb8919f"

 ;; 06 mai 2026 12:02:11
 #:guix-science-commit "4f29b99ce090ed2992d5bc6f3af42180201f5058"
 #:nonguix-commit      "5f2630e69fbbe9e79c350a67545f0fef7e93e223"
 #:guix-commit         "81934cf7e9263e47f9fd8defa717365592a3473d"

 )

;; It makes no sense to add generation number to the comment. Generation numbers
;; are different on each computer
