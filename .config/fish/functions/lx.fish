# -*- mode: fish -*-

## fish -n lx.fish
## fish_indent --check lx.fish

function lx --description "List executables in current dir (find -perm -111)"
    trace find -maxdepth 1 -perm -111 -type f $argv
end
