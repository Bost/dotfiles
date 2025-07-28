function v --description "vim …"
    set cmd vim (string escape -- $argv)
    echo $cmd
    eval $cmd
end
