function crep
    set f1 $dev/cheatsheet/cmds/linux.sh
    set f2 $dev/cheatsheet/cmds/systemd.sh
    set f3 $dev/cheatsheet/cmds/rest.sh
    # set prm '{:cmt-str "#" :files ["'$f1'" "'$f2'" "'$f3'"]}'
    # lumo $dev/dotfiles/lumo/crep.cljs $prm $argv
    if test $argv = "--files"
        echo $f1
        echo $f2
        echo $f3
        return
    end

    # -P --perl-regexp
    # -E --extended-regexp
    # -z --null-data
    # -o --only-matching
    # -e PATTERN, --regepx=PATTERN
    set prms -Pzo
    # insert separating line   | greate matching groups       | grep for desired pattern
    sed 's/^$/\n/' $f1 $f2 $f3 | grep $prms "#.+\n(.*\n)+?\n" | grep -Pzi -e (string escape -- $argv)
end
