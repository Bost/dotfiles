# -*- mode: fish -*-

## fish -n d2u.fish
## fish_indent --check d2u.fish

function d2u --description "fromdos …"
    set cmd fromdos (string escape -- $argv)
    echo $cmd
    eval $cmd
end
