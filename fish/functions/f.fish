function f
    # set cmd find -name "(string escape -- $argv)"
    # -iname case insensitive
    set cmd find -iname "(string escape -- $argv)"
    eval $cmd
    echo $cmd
end
