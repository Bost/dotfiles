function gicmanoe --description "git commit --amend --no-edit …"
    set cmd git commit --amend --no-edit (string escape -- $argv)
    echo $cmd
    eval $cmd
end
