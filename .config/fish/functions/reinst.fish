# -*- mode: fish -*-

## fish -n reinst.fish
## fish_indent --check reinst.fish

function reinst --description "Reinstall packages (apt --reinstall install)"
    trace sudo apt --reinstall install --yes $argv
end
