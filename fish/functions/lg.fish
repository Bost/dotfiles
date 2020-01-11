function lg
    set cmd git lg-20 (string escape -- $argv)
    # the '--all' takes too long to execute
    # set cmd git lg-all-20 (string escape -- $argv)
    echo $cmd
    eval $cmd
    echo "### See: git lg-all-20"
end
