# -*- mode: fish -*-

## fish -n lat.fish
## fish_indent --check lat.fish

function lat --description "List by modification time, newest first (ls -lt)"
    trace ls -lt -all "--time-style=+%d-%m-%Y %H:%M:%S" $argv
end
