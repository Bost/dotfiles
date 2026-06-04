# -*- mode: fish -*-

## fish -n fscm.fish
## fish_indent --check fscm.fish

function fscm --description "fd --extension scm …"
    set cmd fd --extension scm (string escape -- $argv)
    echo $cmd
    eval $cmd
end
