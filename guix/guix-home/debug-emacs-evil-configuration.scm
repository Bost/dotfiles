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
        "bash@5.1.8"
        "git@2.36.1"
        "emacs@28.1"
        "emacs-evil@1.15.0-0.008a6cd"
        "emacs-magit@3.3.0"
        "emacs-evil-collection@0.0.8"
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
