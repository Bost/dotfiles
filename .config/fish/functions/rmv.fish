# -*- mode: fish -*-

## fish -n rmv.fish
## fish_indent --check rmv.fish

function rmv --description "Remove a snap or apt package (snap/apt remove)"
    trace snap list $argv
    if test $status = 0
        trace sudo snap remove $argv
    else
        trace sudo apt remove --yes $argv
    end
end
