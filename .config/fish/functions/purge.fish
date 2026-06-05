# -*- mode: fish -*-

## fish -n purge.fish
## fish_indent --check purge.fish

function purge --description "Remove packages and their config (apt purge)"
    trace sudo apt purge --yes $argv
end
