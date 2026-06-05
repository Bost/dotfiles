# -*- mode: fish -*-

## fish -n take.fish
## fish_indent --check take.fish

function take --description "Create a directory and cd into it (mkcd)"
    mkcd $argv
end
