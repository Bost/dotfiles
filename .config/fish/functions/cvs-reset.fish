# -*- mode: fish -*-

## fish -n cvs-reset.fish
## fish_indent --check cvs-reset.fish

function cvs-reset --description "cvs update -C -l -d -P …"
    set cmd cvs update -C -l -d -P (string escape -- $argv)
    echo $cmd
    eval $cmd
end
