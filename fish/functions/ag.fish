function ag
    set cmd /usr/bin/ag (string escape -- $argv)
    echo $cmd
    eval $cmd
    # echo "##########################################"
    # echo "### See also ripgrep, ack, pt, grep, sift"
end
