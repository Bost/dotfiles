# -*- mode: fish -*-

## fish -n lcxa.fish
## fish_indent --check lcxa.fish

function lcxa --description "lein cljx auto …"
    set cmd lein cljx auto (string escape -- $argv)
    echo $cmd
    eval $cmd
end
