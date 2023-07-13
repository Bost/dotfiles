function gr --description "git rebase"
    set cmd git rebase $argv
    # set cmd git rebase (string escape -- $argv)
    echo $cmd
    eval $cmd
end
