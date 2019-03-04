function ag
    set cmd /usr/bin/ag (string escape -- $argv)
    echo $cmd
    # echo "######## See also: ripgrep, ack, pt, grep, sift"
    eval $cmd
end
