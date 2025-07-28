function gipl --description "git pull …"
    set cmd git pull (string escape -- $argv)
    echo $cmd
    eval $cmd
end
