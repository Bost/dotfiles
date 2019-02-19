function cre
    set f1 $dev/cheatsheet/cmds/emacs.el
    # ack-cheat $f1 $argv
    set prm '{:cmt-str ";" :files ["'$f1'"]}'
    # lumo $dev/dotfiles/lumo/crep.cljs $prm $argv
    if test $argv = "--files"
        echo $f1
        return
    end
    set cmd "grep $crep0 $lispline $f1 | grep $crep1" (string escape -- $argv)
    eval $cmd
    # echo $cmd
end
