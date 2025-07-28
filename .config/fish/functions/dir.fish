function dir --description "ls --color=auto --format=vertical …"
    set cmd ls --color=auto --format=vertical (string escape -- $argv)
    echo $cmd
    eval $cmd
end
