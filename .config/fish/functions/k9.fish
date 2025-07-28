function k9 --description "kill -9 …"
    set cmd kill -9 (string escape -- $argv)
    echo $cmd
    eval $cmd
end
