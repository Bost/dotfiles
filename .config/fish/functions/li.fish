# -*- mode: fish -*-

## fish -n li.fish
## fish_indent --check li.fish

function li --description "Install project to local repo (lein install)"
    trace lein install $argv
end
