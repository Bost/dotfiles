# -*- mode: fish -*-

## fish -n k9.fish
## fish_indent --check k9.fish

function k9 --description "Force-kill processes by PID (kill -9)"
    trace kill -9 $argv
end
