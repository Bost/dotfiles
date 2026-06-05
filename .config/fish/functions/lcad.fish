# -*- mode: fish -*-

## fish -n lcad.fish
## fish_indent --check lcad.fish

function lcad --description "Auto-rebuild ClojureScript, dev profile (lein cljsbuild auto dev)"
    trace lein cljsbuild auto dev $argv
end
