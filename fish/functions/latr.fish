function latr --description "Sort by modification time, newest first, reversed"
    # set cmd ls -latr (string escape -- $argv)
    set cmd ls --sort=time -l --almost-all --reverse (string escape -- $argv)
    eval $cmd
    echo $cmd
end
