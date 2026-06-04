# -*- mode: fish -*-

## fish -n ll.fish
## fish_indent --check ll.fish

function ll --description "ls -lh …"
    set cmd ls -lh (string escape -- $argv)
    echo $cmd
    eval $cmd
end
