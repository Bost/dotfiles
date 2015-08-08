function inst
    set cmd "sudo apt-get install --yes $argv"
    echo $cmd
    eval $cmd
end
