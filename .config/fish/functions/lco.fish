# -*- mode: fish -*-

## fish -n lco.fish
## fish_indent --check lco.fish

function lco --description "lein cljsbuild once …"
    set cmd lein cljsbuild once (string escape -- $argv)
    echo $cmd
    eval $cmd
end
