function ys
    set cmd youtube-dl --extract-audio (string escape -- $argv)
    echo $cmd
    eval $cmd
end
