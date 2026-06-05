# -*- mode: fish -*-

## fish -n v.fish
## fish_indent --check v.fish

function v --description "Edit files (vim)"
    trace vim $argv
end
