function ag
    set cmd /usr/bin/ag (string escape -- $argv)
    eval $cmd
    echo $cmd
    echo "######## See also: ripgrep, ack, pt, grep, sift"
end
