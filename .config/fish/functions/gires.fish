function gires --description "git rebase --skip …"
    set cmd git rebase --skip (string escape -- $argv)
    echo $cmd
    eval $cmd
end
