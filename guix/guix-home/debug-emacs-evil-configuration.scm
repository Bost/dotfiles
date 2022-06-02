(use-modules
 (gnu home)
 (gnu packages)
 (gnu services)
 (guix gexp)
 (gnu home services)
 (gnu home services shells))

(home-environment
 (packages
  (map (compose list specification->package+output)
       (list
        "bash"
        "git"
        "emacs"
        "emacs-evil"
        "emacs-magit"
        "emacs-evil-collection"
        )))
 (services
  (list (service
         home-bash-service-type
         (home-bash-configuration
          (aliases
           '(("g" . "create_test_repo")))
          (bashrc
           (list (local-file
                  "/home/bost/dev/dotfiles/guix/guix-home/.bashrc"
                  "bashrc")))
          (bash-profile
           (list (local-file
                  "/home/bost/dev/dotfiles/guix/guix-home/.bash_profile"
                  "bash_profile")))))

        (simple-service 'emacs-config
                        home-files-service-type
                        (list `(".emacs.d/init.el"
                                ,(local-file
                                  "/home/bost/dev/dotfiles/guix/guix-home/.emacs.d/init.el"
                                  "emacs-init.el"))))
        )))
