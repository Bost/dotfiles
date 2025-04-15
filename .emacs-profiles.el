;; See https://github.com/plexus/chemacs2
;; association list:
;; keys/cars - profile names, values/cdrs - profile configurations
;;
;; Can't use constructs like (concat dir "spacemacs", since the
;; .emacs-profiles.el is not evaluated
(("develop" .
  ((user-emacs-directory . "~/.emacs.d.distros/spacemacs/develop/src")
   (server-name . "develop")
   (env . (("SPACEMACSDIR" .
            "~/.emacs.d.distros/spacemacs/develop/cfg")))))

 ("guix" .
  ((user-emacs-directory . "~/.emacs.d.distros/spacemacs/guix/src")
   (server-name . "guix")
   (env . (("SPACEMACSDIR" .
            "~/.emacs.d.distros/spacemacs/guix/cfg")))))

 ("crafted" .
  ((user-emacs-directory . "~/.emacs.d.distros/crafted-emacs")
   (server-name . "crafted")
   (env . (("CRAFTED_EMACS_HOME" .
            "~/.emacs.d.distros/crafted-emacs/personal"))))))
