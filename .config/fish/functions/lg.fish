function lg --description "Show last 20 git logs: git lg-20 …"
    set cmd git lg-20 (string escape -- $argv)
    echo $cmd
    eval $cmd
end
