# -*- mode: fish -*-

## fish -n fjar.fish
## fish_indent --check fjar.fish

function fjar --description "fd --extension jar …"
    set cmd fd --extension jar (string escape -- $argv)
    echo $cmd
    eval $cmd
end
