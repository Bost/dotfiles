function hrep
    set cmd "history | grep $argv"
    echo $cmd
    eval $cmd
end
