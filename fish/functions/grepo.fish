function grepo --description "Search in org-files. See also grepl.fish"
    # example: grepo "\<project\>"
    set fdirs "--exclude-dir={.git,target} --include=\*.{org}"
    # wdir is undefined i.e. examine the working directory
    # set wdir ./
    set cmd grep $optsGrepC $fdirs (string escape -- $argv) $wdir
    eval $cmd
    echo "#######################################################"
    echo $cmd
end
