# -*- mode: fish -*-

## fish -n cvs-reset.fish
## fish_indent --check cvs-reset.fish

function cvs-reset --description "Reset CVS working copy (cvs update -CldP)"
    trace cvs update -C -l -d -P $argv
end
