# -*- mode: fish -*-

## fish -n rmrf.fish
## fish_indent --check rmrf.fish

function rmrf --description "rm -rf …"
    set cmd rm -rf (string escape -- $argv)
    echo $cmd
    echo "Not executing - removing is dangerous"
    # eval $cmd
end
