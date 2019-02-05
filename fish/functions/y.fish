function y
    set cmd youtube-dl --write-auto-sub --sub-lang fr (string escape -- $argv)
    echo $cmd
    eval $cmd
end
