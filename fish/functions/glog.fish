function glog
    set escArgv (string escape -- $argv)

    for remote in $remotes
        # echo "before status:" $status
        # i.e. git pull origin $escArgv; and git fetch --tags
        # where:
        #      git fetch --tags
        # Fetch all tags from the $remote i.e. fetch remote tags refs/tags/*
        # into local tags with the same name, in addition to whatever else
        # would otherwise be fetched.
        set cmd git fetch --tags $remote $escArgv
        echo $cmd
        eval $cmd
        if test $status = 0
            set cmd git rebase $escArgv
            echo $cmd
            eval $cmd
        end
        if test $status != 0
            break
        end
    end
end
