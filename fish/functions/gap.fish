function gap
    set cmd "git add -p $argv"
    echo $cmd
    eval $cmd
end
