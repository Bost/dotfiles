function crg
    # ack-cheat $dev/cheatsheet/cmds/git.sh $argv
    set f1 $dev/cheatsheet/cmds/git.sh
    # set prm '{:cmt-str "#" :files ["'$f1'"]}'
    # lumo $dev/dotfiles/lumo/crep.cljs $prm $argv

    # insert separating line | greate matching groups | grep for desired pattern
    sed $sed0 $f1 | grep $crep0 $shellline | grep $crep1 (string escape -- $argv)
end
