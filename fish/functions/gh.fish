function gh
    set escArgv (string escape -- $argv)
    set cmd git push --verbose $escArgv
    # set cmd git push --verbose master:refs/heads/master
    echo $cmd
    eval $cmd
end
