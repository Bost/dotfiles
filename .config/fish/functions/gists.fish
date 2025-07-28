function gists --description "git status --short …"
    set cmd git status --short (string escape -- $argv)
    echo $cmd
    eval $cmd
end
