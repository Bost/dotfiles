function crg
    # ack-cheat $dev/cheatsheet/cmds/git.sh $argv
    set f1 $dev/cheatsheet/cmds/git.sh
    # set prm '{:cmt-str "#" :files ["'$f1'"]}'
    # lumo $dev/dotfiles/lumo/crep.cljs $prm $argv

    # -P --perl-regexp
    # -E --extended-regexp
    # -z --null-data
    # -o --only-matching
    # -e PATTERN, --regepx=PATTERN
    set prms -Pzo
    # insert separating line | greate matching groups       | grep for desired pattern
    sed 's/^$/\n/' $f1       | grep $prms "#.+\n(.*\n)+?\n" | grep -Pzi -e (string escape -- $argv)
end
