function h
    set cmd history --show-time="[%Y-%m-%d %H:%M:%S]\ " (string escape -- $argv)
    echo $cmd
    eval $cmd
end
