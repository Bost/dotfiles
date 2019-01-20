function cg
    # example: cg "\<project\>"

    # string delims needed - because of the $argv in the middle
    set cmd "grep -nir \"$argv\" --exclude-dir={.git,target} --include=\*.{clj,cljs,cljc} ./"
    echo $cmd
    eval $cmd
end
