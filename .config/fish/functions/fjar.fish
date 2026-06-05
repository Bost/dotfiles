# -*- mode: fish -*-

## fish -n fjar.fish
## fish_indent --check fjar.fish

function fjar --description "Find .jar files (fd --extension jar)"
    trace fd --extension jar $argv
end
