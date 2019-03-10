function f
    set cmd find -name "(string escape -- $argv)"
    eval $cmd
    echo $cmd
end
