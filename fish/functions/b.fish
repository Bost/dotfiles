function c
    set escArgv (string escape -- $argv)
    if test -z "$escArgv"
        # echo "Udefined or empty escArgv:" $escArgv
        l
    else if test -d $escArgv # is it a directory?
        l $estArgv
    else if test -f $estArgv # is it a regular file?
        set cmd bat $escArgv
        # echo $cmd # otherwise c <file> | jq '.' doesn't work
        eval $cmd
        # is a directory?
    else
        crep $escArgv $files
    end
end
