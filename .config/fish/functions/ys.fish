function ys --description "youtube-dl --extract-audio …"
    set cmd youtube-dl --extract-audio (string escape -- $argv)
    echo $cmd
    eval $cmd
end
