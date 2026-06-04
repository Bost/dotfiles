# -*- mode: fish -*-

## fish -n lat.fish
## fish_indent --check lat.fish

function lat --description "Sort by modification time, newest first, reversed"
    # set cmd ls -lat (string escape -- $argv)
    set cmd ls -lt -all "--time-style='+%d-%m-%Y %H:%M:%S'" (string escape -- $argv)
    eval $cmd
    echo $cmd
end
