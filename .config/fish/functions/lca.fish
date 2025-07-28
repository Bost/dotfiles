function lca --description "lein cljsbuild auto …"
    set cmd lein cljsbuild auto (string escape -- $argv)
    echo $cmd
    eval $cmd
end
