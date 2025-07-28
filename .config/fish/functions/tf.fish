function tf --description "tail --follow …"
    set cmd tail --follow (string escape -- $argv)
    echo $cmd
    eval $cmd
end
