function take
    # this doesn't work
    # set cmd0 mkdir -p (string escape -- $argv); and cd (string escape -- $argv)
    set cmd0 mkdir -p (string escape -- $argv)
    echo $cmd0
    eval $cmd0
    if test -d (string escape -- $argv)
        set cmd1 cd (string escape -- $argv)
        echo $cmd1
        eval $cmd1
    end
end
