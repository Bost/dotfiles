;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules
 (gnu home)
 (gnu packages)
 (gnu services)
 (guix gexp)
 (gnu home services)
 (gnu home services shells))

(define (ghc s)
  "Prepend 'guix-home-config-directory' to `s'
Note:
(format #t \"~a\" \"foo\") doesn't work"
  (string-append (getenv "HOME") "/dev/dotfiles/guix/guix-home/" s))

(home-environment
 (packages
  (map (compose list specification->package+output)
       (list
        "direnv"
        "bash"
        "fish"
        "git"
        "htop"

        "emacs"
        "emacs-evil"
        "emacs-magit"

        ;; manage guix profiles
        "emacs-guix"

        ;; procps contains: free, pgrep, pidof, pkill, pmap, ps, pwdx, slabtop,
        ;; tload, top, vmstat, w, watch, sysctl
        "procps"

        ;; findutils contains: find
        "findutils"
        )))
 (services
  (list (service
         home-bash-service-type
         (home-bash-configuration
          (aliases
           '(("grep" . "grep --color=auto")
             ("l" . "ls -lA --color=auto")
             ("ll" . "ls -l")
             ("ls" . "ls -p --color=auto")))
          (bashrc
           (list (local-file
                  (ghc "/.bashrc" )
                  "bashrc")))
          (bash-profile
           (list (local-file
                  (ghc "/.bash_profile")
                  "bash_profile")))))

        (simple-service
         'test-config
         home-files-service-type
         (list `("config/test.conf"
                 ,(plain-file "tmp-file.txt"
                              "the content of ~/.config/test.conf"))))

        (simple-service 'emacs-config
                        home-files-service-type
                        (list `(".emacs.d/init.el"
                                ,(local-file
                                  (ghc ".emacs.d/init.el")
                                  "emacs-init.el"))))
        )))
