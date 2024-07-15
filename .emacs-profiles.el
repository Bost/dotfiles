;; git clone https://github.com/plexus/chemacs2.git ~/.emacs.d

;; Can't use constructs like (concat dir "spacemacs"), since the
;; .emacs-profiles.el is not evaluated
(
 ("default" .
  ((user-emacs-directory . "~/.emacs.d.distros/default")
   (server-name . "gnu")
   ))

 ("prelude" .
  ((user-emacs-directory . "~/.emacs.d.distros/prelude")
   (server-name . "prelude")
   ))

 ("spguimacs" .
  ((user-emacs-directory . "~/.emacs.d.distros/spguimacs")
   (server-name . "spguimacs")
   ;; mv /path/to/.spguimacs $SPGUIMACSDIR/init.el
   (env . (
           ;; ("EMACSLOADPATH" .
           ;;  '())
           ("SPGUIMACSDIR" .
            "~/.emacs.d.distros/spguimacs-config"
            ;; "~/.emacs.d.distros/spguimacs-default-config"
;;; Following leads to:
;;; Error loading .spacemacs: (error "Recursive load" ... )
            ;; "~/.emacs.d.distros/spguimacs"
            )))
   ))

 ("spacemacs" .
  ((user-emacs-directory . "~/.emacs.d.distros/spacemacs")
   (server-name . "spacemacs")
   ;; mv /path/to/.spguimacs $SPACEMACSDIR/init.el
   (env . (
           ;; ("EMACSLOADPATH" .
           ;;  ;; parse-colon-path
           ;;  '(
           ;;   "~/.guix-profile/share/emacs/site-lisp"
           ;;   "/gnu/store/2aai9453k47w0ar821bhqhpv2k69v064-emacs-29.4/share/emacs/29.4/lisp"
           ;;   ))
           ("SPACEMACSDIR" .
            "~/.emacs.d.distros/spacemacs-config")))
   ))

 ("practicalli" .
  ((user-emacs-directory . "~/.emacs.d.distros/spacemacs-practicalli")
   (server-name . "practicalli")
   ;; location of .spacemacs and init.el
   ;; (env . (("SPACEMACSDIR" . "~/.spacemacs.d")))
   ))

 ;; cd <user-emacs-directory>
 ;; ./bin/doom install --doomdir=<DOOMDIR>
 ;; # i.e.
 ;; mkdir ~/.config/doom
 ;; cd ~/.emacs.d.distros/doom-emacs
 ;; ./bin/doom install --doomdir ~/.config/doom
 ("doom" .
  ((user-emacs-directory . "~/.emacs.d.distros/doom-emacs")
   (server-name . "doom")
   (env . (("DOOMDIR" .
            ;; "~/.config/doom-emacs" ; doesn't work
            "~/.config/doom")))))

 ("crafted" .
  ((user-emacs-directory . "~/.emacs.d.distros/crafted-emacs")
   (server-name . "crafted")
   (env . (("CRAFTED_EMACS_HOME" .
            "~/.emacs.d.distros/crafted-emacs/personal")))))
 )
