function c
    set escArgv (string escape -- $argv)
    if test -z "$escArgv"
        # echo "Udefined or empty escArgv:" $escArgv
        l
    else if test -d $escArgv # is it a directory?
        l $escArgv
    else if test -f $escArgv # is it a regular file?
        if test -f /usr/bin/bat
            set binary bat
        else
            echo "# TODO install `bat`"
            echo "#####################"
            set binary cat
        end
        set cmd $binary $escArgv
        # echo $cmd # otherwise c <file> | jq '.' doesn't work
        eval $cmd
        # is a directory?
    else
        crep $escArgv $files
    end
end
