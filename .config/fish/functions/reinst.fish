# -*- mode: fish -*-

## fish -n reinst.fish
## fish_indent --check reinst.fish

function reinst --description "sudo apt --reinstall install --yes …"
    set cmd sudo apt --reinstall install --yes (string escape -- $argv)
    echo $cmd
    eval $cmd
end
