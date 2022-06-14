function f
    # example: f 'clj$'
    if test (which fd 2> /dev/null)
        set fdBin fd
    else if test (which fdfind 2> /dev/null)
        set fdBin fdfind
    else
        printf "ERR: neither fd nor fdfind installed\n"
    end
    set cmd $fdBin (string escape -- $argv)
    eval $cmd
    # echo "#" $cmd
end
