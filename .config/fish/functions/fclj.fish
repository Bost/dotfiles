# -*- mode: fish -*-

## fish -n fclj.fish
## fish_indent --check fclj.fish

function fclj --description "fd --extension clj …"
    set cmd fd --extension clj (string escape -- $argv)
    echo $cmd
    eval $cmd
end
