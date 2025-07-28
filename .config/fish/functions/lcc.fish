function lcc --description "lein clean …"
    set cmd lein clean (string escape -- $argv)
    echo $cmd
    eval $cmd
end
