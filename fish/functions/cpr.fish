function cpr --description "cp -r : Copy directories recursively"
    # TODO cpr doesn't work with sudo:
    #     sudo cpr /boot/grub/ /tmp
    set cmd cp -r (string escape -- $argv)
    echo $cmd
    eval $cmd
end
