function giadd. --description "git add . …"
    set cmd git add . (string escape -- $argv)
    echo $cmd
    eval $cmd
end
