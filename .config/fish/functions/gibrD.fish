function gibrD --description "git branch --force --delete …"
    set cmd git branch --force --delete (string escape -- $argv)
    echo $cmd
    eval $cmd
end
