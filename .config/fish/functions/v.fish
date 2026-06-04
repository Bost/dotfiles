# -*- mode: fish -*-

## fish -n v.fish
## fish_indent --check v.fish

function v --description "vim …"
    set cmd vim (string escape -- $argv)
    echo $cmd
    eval $cmd
end
