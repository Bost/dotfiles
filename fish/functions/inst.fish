function inst
    set cmd sudo snap install (string escape -- $argv)
    # set cmd sudo apt install --yes (string escape -- $argv)
    echo $cmd
    eval $cmd
end
