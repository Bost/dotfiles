function inst
    set cmd sudo snap install (string escape -- $argv)
    echo $cmd
    eval $cmd

    if test $status != 0
        set cmd sudo apt install --yes (string escape -- $argv)
        echo $cmd
        eval $cmd
    end
end
