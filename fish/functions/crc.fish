function crc
    # ack-cheat $dev/cheatsheet/cmds/clojure.clj $argv
    set f1 $dev/cheatsheet/clj/src/clj/core.clj
    # set prm '{:cmt-str ";" :files ["'$f1'"]}'
    # lumo $dev/dotfiles/lumo/crep.cljs $prm $argv

    # -P --perl-regexp
    # -z --null-data
    # -o --only-matching
    # insert separating line   | greate matching groups  | grep for desired pattern
    sed 's/^$/\n/' $f1 $f2 $f3 | grep -Pzo "\n(\S.*\n)*" | grep -Pz $argv
end
