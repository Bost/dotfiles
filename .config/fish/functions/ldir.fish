# -*- mode: fish -*-

## fish -n ldir.fish
## fish_indent --check ldir.fish

function ldir --description "List subdirectories only (incl. hidden), no files"
    # `*/` and `.*/` match directories only (never plain files). Expand via `set`
    # so an empty match yields an empty list instead of fish's "No matches" error.
    set --local dirs */ .*/
    # `.*/` always matches ./ and ../ — drop them.
    set dirs (string match --invert --regex '^\.\.?/$' -- $dirs)
    if set --query dirs[1]
        trace ls --color=auto -d -l $dirs
    else
        echo "No subdirectories here."
    end
end
