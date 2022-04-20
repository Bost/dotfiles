(use-modules (ice-9 rdelim)
             (ice-9 regex)
             (ice-9 popen)
             (utils)
             (guix build utils) #| invoke |#
             )

(define (main args)
  (let* ((full-hash (cadr args))
         (hash (substring full-hash 0 10))
         (wd (string-append "/home/bost/dev/emacs-" hash))
         (gd "/home/bost/dev/emacs/.git")

         )
    (format #t "~a\n" hash)
    (exec "echo foo")
    #;(invoke "mkdir" wd)
    )
  #;(map exec
  (list
  (git "fetch" "--tags" "origin" "develop")
  (git "rebase" "origin/develop" "develop")
  (git "rebase" "develop" "cycle"))))

#|
fullHash=$1
hash=${fullHash:0:10}

wd=/home/bost/dev/emacs-$hash
gd=/home/bost/dev/emacs/.git

echo mkdir -p $wd
mkdir -p $wd

echo git --git-dir=$gd --work-tree=$wd checkout $hash
git --git-dir=$gd --work-tree=$wd checkout $hash
echo git --git-dir=$gd --work-tree=$wd reset --hard
git --git-dir=$gd --work-tree=$wd reset --hard
echo guix hash --serializer=nar -x $wd

base32=$(guix hash --serializer=nar -x $wd)

gp=/home/bost/dev/guix-playground
echo sed -i -e "s|hashhash|$hash|" $gp/gnu/packages/emacs.scm
sed -i -e "s|hashhash|$hash|" $gp/gnu/packages/emacs.scm

echo sed -i -e "s|basebase|$base32|" $gp/gnu/packages/emacs.scm
sed -i -e "s|basebase|$base32|" $gp/gnu/packages/emacs.scm

echo guix build --cores=20 --load-path=$gp emacs-next-$hash
guix build --cores=20 --load-path=$gp emacs-next-$hash
|#
