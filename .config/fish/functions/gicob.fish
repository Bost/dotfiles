function gicob --description "git checkout -b …"
    set cmd git checkout -b (string escape -- $argv)
    echo $cmd
    eval $cmd
end
