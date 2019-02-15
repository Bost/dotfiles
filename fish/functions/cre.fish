function cre
    set f1 $dev/cheatsheet/cmds/emacs.el
    # ack-cheat $f1 $argv
    set prm '{:cmt-str ";" :files ["'$f1'"]}'
    # lumo $dev/dotfiles/lumo/crep.cljs $prm $argv
    if test $argv = "--files"
        echo $f1
        return
    end
    # -P, --perl-regexp
    # -z, --null-data
    # -o, --only-matching
    # -e PATTERN, --regepx=PATTERN
    # insert separating line | greate matching groups  | grep for desired pattern
    sed 's/^$/\n/' $f1       | grep -Pzo "\n(\S.*\n)*" | grep -Pz -e (string escape -- $argv)
end
