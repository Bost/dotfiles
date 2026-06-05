# -*- mode: fish -*-

## fish -n fjava.fish
## fish_indent --check fjava.fish

function fjava --description "Find .java files (fd --extension java)"
    trace fd --extension java $argv
end
