function rmv
    set cmd snap list (string escape -- $argv)
    echo $cmd
    eval $cmd
    if test $status = 0
        set cmd sudo snap remove (string escape -- $argv)
    else
        set cmd sudo apt remove --yes (string escape -- $argv)
    end
    echo $cmd
    eval $cmd
end
