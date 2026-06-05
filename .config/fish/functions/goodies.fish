# -*- mode: fish -*-

## fish -n goodies.fish
## fish_indent --check goodies.fish

function goodies --description "List services needing restart (needrestart)"
    # set cmd sudo checkrestart $argv
    trace sudo needrestart $argv
end
