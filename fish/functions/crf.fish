function crf
    # ack-cheat $dev/cheatsheet/cmds/findgrep.sh $argv
    set f1 $dev/cheatsheet/cmds/findgrep.sh
    # set prm '{:cmt-str "#" :files ["'$f1'"]}'
    # lumo $dev/dotfiles/lumo/crep.cljs $prm $argv

    set cmd "grep $crep0 $shellline $f1 | grep $crep1" (string escape -- $argv)
    eval $cmd
    # echo $cmd
end
