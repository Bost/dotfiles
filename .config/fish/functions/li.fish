# -*- mode: fish -*-

## fish -n li.fish
## fish_indent --check li.fish

function li --description "lein install …"
    set cmd lein install (string escape -- $argv)
    echo $cmd
    eval $cmd
end
