# -*- mode: fish -*-

## fish -n synaptic.fish
## fish_indent --check synaptic.fish

function synaptic --description "gksudo synaptic …"
    set cmd gksudo synaptic (string escape -- $argv)
    echo $cmd
    eval $cmd
end
