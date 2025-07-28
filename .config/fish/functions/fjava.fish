function fjava --description "fd --extension java …"
    set cmd fd --extension java (string escape -- $argv)
    echo $cmd
    eval $cmd
end
