# -*- mode: fish -*-

## fish -n k9.fish
## fish_indent --check k9.fish

function k9 --description "kill -9 …"
    set cmd kill -9 (string escape -- $argv)
    echo $cmd
    eval $cmd
end
