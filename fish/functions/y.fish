function y
    set cmd youtube-dl --write-auto-sub $argv
    echo $cmd
    eval $cmd
end
