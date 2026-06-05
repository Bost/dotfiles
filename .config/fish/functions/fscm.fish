# -*- mode: fish -*-

## fish -n fscm.fish
## fish_indent --check fscm.fish

function fscm --description "Find .scm files (fd --extension scm)"
    trace fd --extension scm $argv
end
