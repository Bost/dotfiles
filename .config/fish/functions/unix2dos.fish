function unix2dos --description "todos …"
    set cmd todos (string escape -- $argv)
    echo $cmd
    eval $cmd
end
