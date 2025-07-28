function gife --description "git fetch …"
    set cmd git fetch (string escape -- $argv)
    echo $cmd
    eval $cmd
end
