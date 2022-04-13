function f
    # example: f '*.clj'
    # set cmd find -name (string escape -- $argv)
    # -iname case insensitive
    # set cmd find -iname (string escape -- $argv)
    if test (which fd 2> /dev/null)
        set fdBin fd
    else if test (which fdfind 2> /dev/null)
        set fdBin fdfind
    end
    if test $fdBin
        set cmd $fdBin $argv
        eval $cmd
        # echo "#" $cmd
    else
        printf "ERR: neither fd nor fdfind installed\n"
    end
end
