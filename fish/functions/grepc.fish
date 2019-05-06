function grepc --description "Grep code files"
    # example: grepc "\<project\>"

    # -n, --line-number
    # -i, --ignore-case
    # -r, --recursive
    # -w, --word-regexp

    # set opts -nir
    set opts --ignore-case --line-number --recursive
    set javaExts "el,clj,cljs,cljc,edn,boot,properties,java"
    set restExts "py,org,md,rst,adoc,html"
    set extentions (string join "," $javaExts $restExts)
    set incl "--include=\*.{"$extentions"}"
    # we're searching recursivelly; out of `resources/public/js/compiled` only
    # the `compiled` subdir needs to be ignored
    set excl "--exclude-dir={.git,target,compiled,node_modules}"
    # set excl $excl "--exclude={cljdocs.clj}"
    # wdir is undefined i.e. examine the working directory
    # set wdir ./
    set cmd grep $opts $fdirs $incl $excl (string escape -- $argv) $wdir
    echo $cmd
    eval $cmd
    echo "### TODO repeat w/ --word-regex if nothing found, i.e. \$status == 1"
    echo $cmd
end
