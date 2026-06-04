# -*- mode: fish -*-

## fish -n hibernate.fish
## fish_indent --check hibernate.fish

function hibernate --description "sudo pm-hibernate …"
    set cmd sudo pm-hibernate (string escape -- $argv)
    echo $cmd
    eval $cmd
end
