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
        crep $argv $files
    end
end
