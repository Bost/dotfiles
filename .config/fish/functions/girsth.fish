function girsth --description "git reset --hard …"
    set cmd git reset --hard (string escape -- $argv)
    echo $cmd
    eval $cmd
end
