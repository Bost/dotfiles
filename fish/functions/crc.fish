function crc
    set f1 $dev/cheatsheet/clj/src/clj/cljdocs.clj
    set f2 $dev/cheatsheet/clj/src/clj/cheat.clj
    # ack-cheat $f1 $f2 $argv
    # set prm '{:cmt-str ";" :files ["'$f1'" "'$f2'"]}'
    # lumo $dev/dotfiles/lumo/crep.cljs $prm $argv
    if test $argv = "--files"
        echo $f1
        echo $f2
        return
    end
    set files $f1
    set cmd "grep $crep0 $lispline $files | grep $crep1" (string escape -- $argv)
    eval $cmd
    echo $cmd
    set files $f2
    echo "============================================== $files"
    set cmd "grep $crep0 $lispline $files | grep $crep1" (string escape -- $argv)
    eval $cmd
    echo "########"
    echo $cmd
end
