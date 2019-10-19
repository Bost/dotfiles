function grepp --description "Grep for python files. See also grepc.fish"
    # example: grepp "\<python\>"
    set fdirs "--exclude-dir={.git,target} --include=\*.{py}"
    # wdir is undefined i.e. examine the working directory
    # set wdir ./
    set cmd grep $optsGrepC $fdirs (string escape -- $argv) $wdir
    echo $cmd
    eval $cmd
end
