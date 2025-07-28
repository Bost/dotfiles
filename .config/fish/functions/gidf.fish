function gidf --description "git diff --word-diff delete …"
    set cmd git diff --word-diff (string escape -- $argv)
    echo $cmd
    eval $cmd
end
