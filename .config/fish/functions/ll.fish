function ll --description "ls -lh …"
    set cmd ls -lh (string escape -- $argv)
    echo $cmd
    eval $cmd
end
