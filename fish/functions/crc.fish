function crc
    set f1 $dev/cheatsheet/clj/src/clj/cljdocs.clj
    set f2 $dev/cheatsheet/clj/src/clj/cheat.clj
    # ack-cheat $f1 $f2 $argv
    # set prm '{:cmt-str ";" :files ["'$f1'" "'$f2'"]}'
    # lumo $dev/dotfiles/lumo/crep.cljs $prm $argv
    if test $argv = "--files"
        echo $f1
        echo $f2
        return
    end
    # insert separating line | greate matching groups | grep for desired pattern
    sed $sed0 $f1 | grep $crep0 $lispline | grep $crep1 (string escape -- $argv)
    echo "============================================== $f2"
    sed $sed0 $f2 | grep $crep0 $lispline | grep $crep1 (string escape -- $argv)
end

