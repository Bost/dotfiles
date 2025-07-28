function gilo --description "git log …"
    set cmd git log (string escape -- $argv)
    echo $cmd
    eval $cmd
end
