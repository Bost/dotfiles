function gishp --description "git stash pop …"
    set cmd git stash pop (string escape -- $argv)
    echo $cmd
    eval $cmd
end
