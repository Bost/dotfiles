# -*- mode: fish -*-

## fish -n purge.fish
## fish_indent --check purge.fish

function purge --description "sudo apt purge --yes …"
    set cmd sudo apt purge --yes (string escape -- $argv)
    echo $cmd
    eval $cmd
end
