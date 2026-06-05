# -*- mode: fish -*-

## fish -n fclj.fish
## fish_indent --check fclj.fish

function fclj --description "Find .clj files (fd --extension clj)"
    trace fd --extension clj $argv
end
