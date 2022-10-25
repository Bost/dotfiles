function gb
    set cmd git branch $argv
    # set cmd git branch (string escape -- $argv)
    echo $cmd
    eval $cmd
end
