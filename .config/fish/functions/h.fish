# -*- mode: fish -*-

## fish -n h.fish
## fish_indent --check h.fish

function h --description "Show command history with timestamps (history)"
    set escArgv $argv
    set cmd (printf "history --show-time=\"%s\" %s" $HISTTIMEFORMAT $escArgv)
    echo $cmd
    eval $cmd
end
