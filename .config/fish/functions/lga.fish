function lga --description "Show all git logs: git lg …"
    set cmd git lg (string escape -- $argv)
    echo $cmd
    eval $cmd
end
