# -*- mode: fish -*-

## fish -n lca.fish
## fish_indent --check lca.fish

function lca --description "lein cljsbuild auto …"
    set cmd lein cljsbuild auto (string escape -- $argv)
    echo $cmd
    eval $cmd
end
