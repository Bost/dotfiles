function rmrf
    set cmd rm -rf (string escape -- $argv)
    echo $cmd
    echo "Not executing - removing is dangerous"
    # eval $cmd
end
