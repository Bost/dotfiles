(define-module (cfg packages all)
  #:export (
            packages-to-install
            ))

(define packages-to-install
  (list
;;; Install git & rsync system-wide to be able to git-clone / rsync the dotfiles
   "git"
   "gparted"    #| disk partition |#
   "mtools"     #| used by gparted |#
   "nss-certs"  #| HTTPS access |#
   "rsync"      #| 'scp' is preinstalled |#
   "vim"        #| 'vi' is preinstalled |#))
