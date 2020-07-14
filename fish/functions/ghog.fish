function ghog
    set escArgv (string escape -- $argv)

    for remote in $remotes
        set cmd git push --follow-tags --verbose $remote $escArgv
        # set cmd git push --follow-tags --verbose $remote master:refs/heads/master
        echo $cmd
        eval $cmd
        if test $status != 0
            break
        end
    end
end
