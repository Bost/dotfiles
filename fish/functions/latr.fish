function latr --description "Sort by modification time, newest first, reversed"
    # set cmd ls -latr (string escape -- $argv)
    set cmd ls -lt --all --reverse (string escape -- $argv)
    eval $cmd
    echo $cmd
end
