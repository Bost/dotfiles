function crep
    set f1 $dev/cheatsheet/cmds/linux.sh
    set f2 $dev/cheatsheet/cmds/systemd.sh
    set f3 $dev/cheatsheet/cmds/rest.sh
    # set prm '{:cmt-str "#" :files ["'$f1'" "'$f2'" "'$f3'"]}'
    # lumo $dev/dotfiles/lumo/crep.cljs $prm $argv

    # -P --perl-regexp
    # -E --extended-regexp
    # -z --null-data
    # -o --only-matching
    set prms -Pzo
    # insert separating line   | greate matching groups   | grep for desired pattern
    sed 's/^$/\n/' $f1 $f2 $f3 | grep $prms "\n(\S.*\n)*" | grep -Pz $argv
end
