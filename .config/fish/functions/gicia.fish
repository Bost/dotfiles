function gicia --description "git commit --amend …"
    set cmd git commit --amend (string escape -- $argv)
    echo $cmd
    eval $cmd
end
