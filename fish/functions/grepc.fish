function grepc --description "See also search.fish"
    set opts --ignore-case --line-number --recursive
    set fdirs "--exclude-dir={.git} --include=\*.{el,clj,cljs,cljc}"
    set cmd grep $opts $fdirs (string escape -- $argv) ./
    echo $cmd
    eval $cmd
end
