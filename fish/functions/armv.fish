function armv
    set cmd "sudo apt-get autoremove --yes $argv"
    echo $cmd
    eval $cmd
end
