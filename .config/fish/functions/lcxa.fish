# -*- mode: fish -*-

## fish -n lcxa.fish
## fish_indent --check lcxa.fish

function lcxa --description "Auto-process cljx sources (lein cljx auto)"
    trace lein cljx auto $argv
end
