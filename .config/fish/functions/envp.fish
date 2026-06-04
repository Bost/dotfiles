# -*- mode: fish -*-

## fish -n envp.fish
## fish_indent --check envp.fish

function envp --description "Show PATH variable"
    env | grep '^PATH=.*'
end
