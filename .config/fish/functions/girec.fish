function girec --description "git rebase --continue …"
    set cmd git rebase --continue (string escape -- $argv)
    echo $cmd
    eval $cmd
end
