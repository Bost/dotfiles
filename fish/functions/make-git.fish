function make-git --description "Compile, install git code"
    set cntJobs (math (eval nproc) / 2)
    set cmd make --jobs $cntJobs
    echo $cmd
    eval $cmd
    if test $status = 0
        set cmd sudo make --jobs $cntJobs install
        echo $cmd
        eval $cmd
        if test $status = 0
            notify-send (printf "Installed: %s" (git --version))
        end
    end
end
