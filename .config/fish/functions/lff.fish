# -*- mode: fish -*-

## fish -n lff.fish
## fish_indent --check lff.fish

function lff --description "List entries with absolute paths (ls $PWD/*)"
    trace ls -lrt -d -1 $PWD/{*,.*}
end
