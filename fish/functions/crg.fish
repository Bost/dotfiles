function crg
    set f1 $dev/cheatsheet/cmds/git.sh
    set files $f1
    # ack-cheat $files $argv
    # set prm '{:cmt-str "#" :files ["'$f1'"]}'
    # lumo $dev/dotfiles/lumo/crep.cljs $prm $argv

    set cmd "grep $crep0 $shellline $files | grep $crep1" (string escape -- $argv)
    eval $cmd
    echo $cmd
end
