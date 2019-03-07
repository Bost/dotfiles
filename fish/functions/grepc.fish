function grepc --description "grep code; see also grepl.fish"
    # example: grepc "\<project\>"

    # -n, --line-number
    # -i, --ignore-case
    # -r, --recursive

    # set opts -nir
    set opts --ignore-case --line-number --recursive
    set incl "--include=\*.{el,clj,cljs,cljc,org,py,md,rst,adoc}"
    set excl "--exclude-dir={.git,target}"
    # wdir is undefined i.e. examine the working directory
    # set wdir ./
    set cmd grep $opts $fdirs $incl $excl (string escape -- $argv) $wdir
    eval $cmd
    echo "#######################################################"
    echo $cmd
end
