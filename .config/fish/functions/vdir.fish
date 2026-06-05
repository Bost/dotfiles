# -*- mode: fish -*-

## fish -n vdir.fish
## fish_indent --check vdir.fish

function vdir --description "List files, long form (ls --format=long)"
    trace ls --color=auto --format=long $argv
end
