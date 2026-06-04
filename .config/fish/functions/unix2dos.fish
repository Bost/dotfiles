# -*- mode: fish -*-

## fish -n unix2dos.fish
## fish_indent --check unix2dos.fish

function unix2dos --description "todos …"
    set cmd todos (string escape -- $argv)
    echo $cmd
    eval $cmd
end
