function c
    # set f1 $dev/cheatsheet/cmds/test.sh
    # set prm '{:cmt-str "#" :files ["'$f1'"]}'
    # lumo $dev/dotfiles/lumo/crep.cljs $prm (string escape -- $argv)
    if test -e $argv
        set cmd cat (string escape -- $argv)
        # echo $cmd # otherwise c <file> | jq '.' doesn't work
        eval $cmd
    else
        # ack-cheat $dev/cheatsheet/cmds/rest.sh (string escape -- $argv)
        set f1 $dev/cheatsheet/cmds/linux.sh
        set f2 $dev/cheatsheet/cmds/rest.sh
        set f3 $dev/cheatsheet/cmds/findgrep.sh
        set f4 $dev/cheatsheet/cmds/git.sh
        set f5 $dev/cheatsheet/cmds/win.bat
        # set prm '{:cmt-str "#" :files ["'$f1'" "'$f2'" "'$f3'" "'$f4'" "'$f5'"]}'
        # lumo $dev/dotfiles/lumo/crep.cljs $prm (string escape -- $argv)

        # -P --perl-regexp
        # -E --extended-regexp
        # -z --null-data
        # -o --only-matching
        # -e PATTERN, --regepx=PATTERN
        set prms -Pzo

        sed 's/^$/\n/' $f1 $f2 $f3 $f4 $f5 | grep $prms "#.+\n(.*\n)+?\n" | grep -Pzi -e (string escape -- $argv)
        set f1 $dev/cheatsheet/clj/src/clj/core.clj
        set f2 $dev/cheatsheet/cmds/emacs.el
        # set prm '{:cmt-str "#" :files ["'$f1'" "'$f2'"]}'
        # lumo $dev/dotfiles/lumo/crep.cljs $prm (string escape -- $argv)
        sed 's/^$/\n/' $f1 $f2 | grep $prms "#.+\n(.*\n)+?\n" | grep -Pzi -e (string escape -- $argv)

        # set f1 $dev/cheatsheet/cmds/utf8.txt
        # set prm '{:cmt-str "" :files ["'$f1'"]}'
        # lumo $dev/dotfiles/lumo/crep.cljs $prm (string escape -- $argv)
    end
end
