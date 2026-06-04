# -*- mode: fish -*-

## fish -n lff.fish
## fish_indent --check lff.fish

function lff --description "Listing with full paths"
    set cmd ls -lrt -d -1 $PWD/{*,.*}
    echo $cmd
    eval $cmd
end
