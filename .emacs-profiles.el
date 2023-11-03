(
 ("default" .
  ((user-emacs-directory . "~/.emacs.d.default")
   (server-name . "gnu")
   ))

 ("prelude" .
  ((user-emacs-directory . "~/.emacs.d.prelude")
   (server-name . "prelude")
   ))

 ("spguimacs" .
  ((user-emacs-directory . "~/.emacs.d.spguimacs")
   (server-name . "spguimacs")
   ;; location of .spguimacs and init.el
   ;; (env . (("SPGUIMACSDIR" . "...")))
   ))

 ("spacemacs" .
  ((user-emacs-directory . "~/.emacs.d.spacemacs")
   (server-name . "spacemacs")
   ;; location of .spacemacs and init.el
   ;; (env . (("SPACEMACSDIR" . "...")))
   ))

 ("practicalli" .
  ((user-emacs-directory . "~/.emacs.d.spacemacs-practicalli")
   (server-name . "practicalli")
   ;; location of .spacemacs and init.el
   ;; (env . (("SPACEMACSDIR" . "~/.spacemacs.d")))
   ))

("doom" .
 ((user-emacs-directory . "~/.emacs.d.doom-emacs")
  (server-name . "doom")
  (env . (("DOOMDIR" . "~/.config/doom-emacs")))))

 ("crafted" .
  ((user-emacs-directory . "~/.emacs.d.crafted-emacs")
   (server-name . "crafted")
   (env . (("CRAFTED_EMACS_HOME" . "~/.emacs.d.crafted-emacs/personal")))))
 )
