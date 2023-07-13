function glf --description "git fetch --tags; git rebase"
    set escArgv (string escape -- $argv)
    set remote franzi
    set cmd git fetch --tags $remote $escArgs
    echo $cmd
    eval $cmd
    if test $status = 0
        set cmd git rebase $escArgs
        echo $cmd
        eval $cmd
    end
end
