# -*- mode: fish -*-

## fish -n lr.fish
## fish_indent --check lr.fish

function lr --description "rlwrap lein repl …"
    set cmd rlwrap lein repl (string escape -- $argv)
    echo $cmd
    eval $cmd
end
