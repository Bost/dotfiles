function gipS --description "git push --force --verbose …"
    set escArgv (string escape -- $argv)
    set cmd git push --force --verbose $escArgv
    # set cmd git push --verbose master:refs/heads/master
    echo $cmd
    eval $cmd
end
