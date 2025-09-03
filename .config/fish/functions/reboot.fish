function reboot --description "sudo reboot …"
    set cmd sudo reboot (string escape -- $argv)
    echo $cmd
    eval $cmd
end
