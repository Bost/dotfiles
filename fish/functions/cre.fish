function cre
    set f1 $dev/cheatsheet/cmds/emacs.el
    set files $f1
    # ack-cheat $files $argv
    # set prm '{:cmt-str ";" :files ["'$f1'"]}'
    # lumo $dev/dotfiles/lumo/crep.cljs $prm $argv
    if test $argv = "--files"
        echo $files
        return
    end
    set cmd "grep $crep0 $lispline $files | grep $crep1" (string escape -- $argv)
    eval $cmd
    echo $cmd
end
