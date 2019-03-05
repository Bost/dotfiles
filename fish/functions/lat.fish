function lat --description "Sort by modification time, newest first, reversed"
    # set cmd ls -lat (string escape -- $argv)
    set cmd ls -lt -all (string escape -- $argv)
    eval $cmd
    echo $cmd
end
