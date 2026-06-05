# -*- mode: fish -*-

## fish -n lco.fish
## fish_indent --check lco.fish

function lco --description "Build ClojureScript once (lein cljsbuild once)"
    trace lein cljsbuild once $argv
end
