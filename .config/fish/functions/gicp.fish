function gicp --description "git cherry-pick …"
    set cmd git cherry-pick (string escape -- $argv)
    echo $cmd
    eval $cmd
end
