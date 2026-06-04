# -*- mode: fish -*-

## fish -n lc.fish
## fish_indent --check lc.fish

function lc --description "lein clean …"
    set cmd lein clean (string escape -- $argv)
    echo $cmd
    eval $cmd
end
