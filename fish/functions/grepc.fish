function grepc
    set cmd grep -nir (string escape -- $argv) "--exclude-dir={.git} --include=\*.{el,clj,cljs,cljc}" ./
    echo $cmd
    eval $cmd
end
