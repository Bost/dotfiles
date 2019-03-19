function grepc --description "grep code; see also grepl.fish"
    # example: grepc "\<project\>"

    # -n, --line-number
    # -i, --ignore-case
    # -r, --recursive
    # -w, --word-regexp

    # set opts -nir
    set opts --ignore-case --line-number --recursive
    set incl "--include=\*.{el,clj,cljs,cljc,org,py,md,rst,adoc}"
    # we're searching recursivelly; out of `resources/public/js/compiled` only
    # the `compiled` subdir needs to be ignored
    set excl "--exclude-dir={.git,target,compiled,node_modules}"
    # set excl $excl "--exclude={cljdocs.clj}"
    # wdir is undefined i.e. examine the working directory
    # set wdir ./
    set cmd grep $opts $fdirs $incl $excl (string escape -- $argv) $wdir
    eval $cmd
    echo "### TODO repeat w/ --word-regex if nothing found, i.e. \$status == 1"
    echo $cmd
end
