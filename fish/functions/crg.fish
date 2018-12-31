function crg
    # ack-cheat $dev/cheatsheet/cmds/git.sh $argv
    set f1 $dev/cheatsheet/cmds/git.sh
    # set prm '{:cmt-str "#" :files ["'$f1'"]}'
    # lumo $dev/dotfiles/lumo/crep.cljs $prm $argv
    sed 's/^$/\n/' $f1 | grep -Pzo "\n(\S.*\n)*" | grep -Pz $argv
end
