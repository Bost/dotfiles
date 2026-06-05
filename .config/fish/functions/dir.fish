# -*- mode: fish -*-

## fish -n dir.fish
## fish_indent --check dir.fish

function dir --description "List files in columns (ls --format=vertical)"
    trace ls --color=auto --format=vertical $argv
end
