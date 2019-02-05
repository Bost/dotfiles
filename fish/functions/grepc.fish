function grepc --description "See also grepl.fish"
    # example: grepc "\<project\>"

    # -n, --line-number
    # -i, --ignore-case
    # -r, --recursive

    # set opts -nir
    set opts --ignore-case --line-number --recursive
    set fdirs "--exclude-dir={.git,target} --include=\*.{el,clj,cljs,cljc}"
    # wdir is undefined i.e. examine the working directory
    # set wdir ./
    set cmd grep $opts $fdirs (string escape -- $argv) $wdir
    eval $cmd
    echo "#######################################################"
    echo $cmd
end
