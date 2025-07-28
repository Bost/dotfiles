function lx --description "Search executable files in current dir …"
    set cmd find -maxdepth 1 -perm -111 -type f (string escape -- $argv)
    echo $cmd
    eval $cmd
end
