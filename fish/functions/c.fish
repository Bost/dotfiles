function c
    # set f1 $dev/cheatsheet/cmds/test.sh
    # set prm '{:cmt-str "#" :files ["'$f1'"]}'
    # lumo $dev/dotfiles/lumo/crep.cljs $prm (string escape -- $argv)
    if test -e $argv
        set cmd cat (string escape -- $argv)
        # echo $cmd # otherwise c <file> | jq '.' doesn't work
        eval $cmd
    else
        set f1 $dev/cheatsheet/cmds/linux.sh
        set f2 $dev/cheatsheet/cmds/rest.sh
        set f3 $dev/cheatsheet/cmds/findgrep.sh
        set f4 $dev/cheatsheet/cmds/git.sh
        set f5 $dev/cheatsheet/cmds/win.bat
        set files $f1 $f2 $f3 $f4 $f5
        # ack-cheat $files (string escape -- $argv)
        if test $argv = "--files"
            echo $files
            return
        end
        set cmd "grep $crep0 $shellline $files | grep $crep1" (string escape -- $argv)
        eval $cmd
        echo $cmd
    end
end
