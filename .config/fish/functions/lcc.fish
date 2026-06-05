# -*- mode: fish -*-

## fish -n lcc.fish
## fish_indent --check lcc.fish

function lcc --description "Clean Leiningen build artifacts (lein clean)"
    trace lein clean $argv
end
