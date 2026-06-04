# -*- mode: fish -*-

## fish -n take.fish
## fish_indent --check take.fish

function take --description "mkdir … && cd …"
    mkcd $argv
end
