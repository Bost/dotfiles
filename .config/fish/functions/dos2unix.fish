# -*- mode: fish -*-

## fish -n dos2unix.fish
## fish_indent --check dos2unix.fish

function dos2unix --description "fromdos …"
    set cmd fromdos (string escape -- $argv)
    echo $cmd
    eval $cmd
end
