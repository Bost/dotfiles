# -*- mode: fish -*-

## fish -n lr.fish
## fish_indent --check lr.fish

function lr --description "Leiningen REPL with readline (rlwrap lein repl)"
    trace rlwrap lein repl $argv
end
