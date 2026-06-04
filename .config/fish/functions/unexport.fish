# -*- mode: fish -*-

## fish -n unexport.fish
## fish_indent --check unexport.fish

function unexport --description "Unset / Erase variable"
    # unset (string escape -- $argv)
    unset $argv
end
