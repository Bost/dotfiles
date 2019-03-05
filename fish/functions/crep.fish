function crep
    set f1 $dev/cheatsheet/cmds/linux.sh
    set f2 $dev/cheatsheet/cmds/systemd.sh
    set f3 $dev/cheatsheet/cmds/rest.sh
    set files $f1 $f2 $f3
    # set prm '{:cmt-str "#" :files ["'$f1'" "'$f2'" "'$f3'"]}'
    # lumo $dev/dotfiles/lumo/crep.cljs $prm $argv
    if test $argv = "--files"
        echo $f1
        echo $f2
        echo $f3
        return
    end
    set cmd "grep $crep0 $shellline $files | grep $crep1" (string escape -- $argv)
    eval $cmd
    echo "########"
    echo $cmd
end
