# -*- mode: fish -*-

## fish -n armv.fish
## fish_indent --check armv.fish

function armv --description "Remove unused packages (apt autoremove)"
    trace sudo apt autoremove --yes $argv
end
