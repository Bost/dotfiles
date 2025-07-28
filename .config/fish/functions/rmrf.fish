function rmrf --description "rm -rf …"
    set cmd rm -rf (string escape -- $argv)
    echo $cmd
    echo "Not executing - removing is dangerous"
    # eval $cmd
end
