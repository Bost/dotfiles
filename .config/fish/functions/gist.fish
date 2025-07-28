function gist --description "git status …"
    set cmd git status (string escape -- $argv)
    echo $cmd
    eval $cmd
end
