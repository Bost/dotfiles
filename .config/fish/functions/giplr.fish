function giplr --description "git pull --rebase …"
    set cmd git pull --rebase (string escape -- $argv)
    echo $cmd
    eval $cmd
end
