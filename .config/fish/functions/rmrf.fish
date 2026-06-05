# -*- mode: fish -*-

## fish -n rmrf.fish
## fish_indent --check rmrf.fish

function rmrf --description "Preview a recursive force-remove — does not execute (rm -rf)"
    set cmd rm -rf $argv
    echo $cmd
    echo "Not executing - removing is dangerous"
    # eval $cmd
end
