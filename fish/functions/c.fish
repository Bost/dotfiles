function c -d "Better cat"
    # TODO see `od` - dump files in octal and other formats
    set escArgv (string escape -- $argv)
    if test -z $escArgv
        # echo "Udefined or empty escArgv:" $escArgv
        l
    else if test -d $escArgv # is it a directory?
        l $escArgv
    # $escArgv doesn't work for 'file\(1\).ext'
    else if test -f $argv # is it a regular file?
        set cmd /usr/bin/batcat
        if test -x $cmd
            # set cmd batcat
        else
            echo "# TODO install `bat` https://github.com/sharkdp/bat#installation"
            echo "#####################"
            set cmd cat
        end
        set cmd $cmd $escArgv
        # echo $cmd # otherwise c <file> | jq '.' doesn't work
        eval $cmd
        # is a directory?
    else
        # TODO: if $escArgv does not contains 'pathSeparator' then exec crep
        # else "No such file or directory" or try to `ls $escArgv` so the error
        # is indicated by the `ls` command
        crep $escArgv $files
    end
end
