function lg --description "Show all git logs"
    set cmd git lg (string escape -- $argv)
    echo $cmd
    eval $cmd
end
