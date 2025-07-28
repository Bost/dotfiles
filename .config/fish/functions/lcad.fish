function lcad --description "lein cljsbuild auto dev …"
    set cmd lein cljsbuild auto dev (string escape -- $argv)
    echo $cmd
    eval $cmd
end
