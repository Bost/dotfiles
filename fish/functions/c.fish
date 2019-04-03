function c
    # is a regular file?
    if test -f $argv
        set cmd cat (string escape -- $argv)
        # echo $cmd # otherwise c <file> | jq '.' doesn't work
        eval $cmd
    # is a directory?
    else if test -d $argv
        l $argv
    else
        set f1 $dev/cheatsheet/cmds/linux.sh
        set f2 $dev/cheatsheet/cmds/rest.sh
        set f3 $dev/cheatsheet/cmds/findgrep.sh
        set f4 $dev/cheatsheet/cmds/git.sh
        set f5 $dev/cheatsheet/cmds/win.bat
        set files $f1 $f2 $f3 $f4 $f5
        cheat-grep $argv $files
    end
end
