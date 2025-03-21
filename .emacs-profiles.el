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

 ("guix-merge" .
  ((user-emacs-directory . "~/.emacs.d.distros/spacemacs/guix-merge/src")
   (server-name . "guix-merge")
   (env . (("SPACEMACSDIR" .
            "~/.emacs.d.distros/spacemacs/guix-merge/cfg")))))

 ("shorten-name" .
  ((user-emacs-directory . "~/.emacs.d.distros/spacemacs/shorten-name/src")
   (server-name . "shorten-name")
   (env . (("SPACEMACSDIR" .
            "~/.emacs.d.distros/spacemacs/shorten-name/cfg")))))

 ("keyseq" .
  ((user-emacs-directory . "~/.emacs.d.distros/spacemacs/keyseq/src")
   (server-name . "keyseq")
   (env . (("SPACEMACSDIR" .
            "~/.emacs.d.distros/spacemacs/keyseq/cfg")))))

 ("crafted" .
  ((user-emacs-directory . "~/.emacs.d.distros/crafted-emacs")
   (server-name . "crafted")
   (env . (("CRAFTED_EMACS_HOME" .
            "~/.emacs.d.distros/crafted-emacs/personal"))))))
