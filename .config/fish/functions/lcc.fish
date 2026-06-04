# -*- mode: fish -*-

## fish -n lcc.fish
## fish_indent --check lcc.fish

function lcc --description "lein clean …"
    set cmd lein clean (string escape -- $argv)
    echo $cmd
    eval $cmd
end
