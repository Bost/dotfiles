# -*- mode: fish -*-

## fish -n armv.fish
## fish_indent --check armv.fish

function armv --description "sudo apt autoremove --yes …"
    set cmd sudo apt autoremove --yes (string escape -- $argv)
    echo $cmd
    eval $cmd
end
