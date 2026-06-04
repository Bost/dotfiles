# -*- mode: fish -*-

## fish -n u2d.fish
## fish_indent --check u2d.fish

function u2d --description "todos …"
    set cmd todos (string escape -- $argv)
    echo $cmd
    eval $cmd
end
