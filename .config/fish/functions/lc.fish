# -*- mode: fish -*-

## fish -n lc.fish
## fish_indent --check lc.fish

function lc --description "Clean Leiningen build artifacts (lein clean)"
    trace lein clean $argv
end
