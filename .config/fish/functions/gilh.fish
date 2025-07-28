function gilh --description "git lg-head …"
    set cmd git lg-head (string escape -- $argv)
    echo $cmd
    eval $cmd
end
