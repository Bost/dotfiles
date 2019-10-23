function glo
    set escArgv (string escape -- $argv)
    set cmd git fetch --tags origin $escArgv
    echo $cmd
    eval $cmd
    if test $status = 0
        set cmd git rebase $escArgv
        echo $cmd
        eval $cmd
    end
end
