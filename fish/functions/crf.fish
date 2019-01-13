function crf
    # ack-cheat $dev/cheatsheet/cmds/findgrep.sh $argv
    set f1 $dev/cheatsheet/cmds/findgrep.sh
    # set prm '{:cmt-str "#" :files ["'$f1'"]}'
    # lumo $dev/dotfiles/lumo/crep.cljs $prm $argv

    # -P --perl-regexp
    # -z --null-data
    # -o --only-matching
    # insert separating line   | greate matching groups  | grep for desired pattern
    sed 's/^$/\n/' $f1 | grep -Pzo "\n(\S.*\n)*" | grep -Pz $argv
end
