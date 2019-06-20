function gho
    set escArgv (string escape -- $argv)

    set cmd git push --verbose origin $escArgv
    # set cmd git push --verbose origin master:refs/heads/master
    echo $cmd
    eval $cmd

    set cmd git push --verbose gitlab $escArgv
    # set cmd git push --verbose origin master:refs/heads/master
    echo $cmd
    eval $cmd
end
