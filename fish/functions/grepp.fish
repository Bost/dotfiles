function grepp --description "Grep for python files. See also grepc.fish"
    # example: grepp "\<python\>"

    # -n, --line-number
    # -i, --ignore-case
    # -r, --recursive

    # set opts -nir
    set opts --ignore-case --line-number --recursive
    set fdirs "--exclude-dir={.git,target} --include=\*.{py}"
    # wdir is undefined i.e. examine the working directory
    # set wdir ./
    set cmd grep $opts $fdirs (string escape -- $argv) $wdir
    echo $cmd
    eval $cmd
end
