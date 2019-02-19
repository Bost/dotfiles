function cre
    set f1 $dev/cheatsheet/cmds/emacs.el
    # ack-cheat $f1 $argv
    set prm '{:cmt-str ";" :files ["'$f1'"]}'
    # lumo $dev/dotfiles/lumo/crep.cljs $prm $argv
    if test $argv = "--files"
        echo $f1
        return
    end
    # insert separating line | greate matching groups | grep for desired pattern
    sed $sed0 $f1 | grep $crep0 $lispline | grep $crep1 (string escape -- $argv)
end
