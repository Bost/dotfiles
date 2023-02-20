function lT --description "ls content, oldest on top"
    # set escArgv (string escape -- $argv)
    set escArgv $argv
    set cmd ~/scm-bin/lT $escArgv
    echo $cmd
    eval $cmd
end
