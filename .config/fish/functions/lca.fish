# -*- mode: fish -*-

## fish -n lca.fish
## fish_indent --check lca.fish

function lca --description "Auto-rebuild ClojureScript (lein cljsbuild auto)"
    trace lein cljsbuild auto $argv
end
