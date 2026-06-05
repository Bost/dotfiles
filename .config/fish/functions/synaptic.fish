# -*- mode: fish -*-

## fish -n synaptic.fish
## fish_indent --check synaptic.fish

function synaptic --description "Launch Synaptic as root (gksudo synaptic)"
    trace gksudo synaptic $argv
end
