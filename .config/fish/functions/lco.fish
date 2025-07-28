function lco --description "lein cljsbuild once …"
    set cmd lein cljsbuild once (string escape -- $argv)
    echo $cmd
    eval $cmd
end
