#!/usr/bin/env fish
function mkcd --description "mkdir … && cd …"
    set dirName (string escape -- $argv)
    # this doesn't work because of the command composition with ';'
    # set cmd (mkdir -p $dirName; and cd $dirName)

    if not test -d $dirName
        set cmd mkdir -p $dirName
        echo $cmd
        eval $cmd
    end
    if test -d $dirName
        set cmd1 cd $dirName
        echo $cmd1
        eval $cmd1
    end
end
