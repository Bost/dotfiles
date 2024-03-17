(define-module (scm-bin spguimacs-launcher)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  #|
  #:use-module (gnu packages emacs) ; for emacs-output-path
  #:use-module (guix)               ; for open-connection
  ;; Activating the two lines above leads to:
  ;;   In procedure resolve-interface: no code for module (gnu packages emacs)
  ;;   In procedure resolve-interface: no code for module (guix)
  |#
  #:use-module (utils)
  #:use-module (scm-bin emacs-launcher)
  #:export (main))

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ spguimacs-launcher) -s
!#

cd $dotf
./guix/home/common/scm-bin/spguimacs-launcher.scm rest args
./guix/home/common/scm-bin/spguimacs-launcher.scm --profile=spguimacs rest args

./guix/home/common/scm-bin/spguimacs-launcher.scm ~/.emacs.d.distros/spguimacs-config/.spacemacs
./guix/home/common/scm-bin/spguimacs-launcher.scm --profile=spguimacs ~/.emacs.d.distros/spguimacs-config/.spacemacs

|#

(define m (module-name-for-logging))
(evaluating-module)

(define (main args)
  (apply (partial emacs-launcher #:profile "spguimacs")
         (cdr args)))

(module-evaluated)
