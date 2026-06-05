# -*- mode: fish -*-

## fish -n ll.fish
## fish_indent --check ll.fish

function ll --description "List files, long form, human sizes (ls -lh)"
    trace ls -lh $argv
end
