function lt --description "ls content, youngest on top"
    # set escArgv (string escape -- $argv)
    set escArgv $argv
    set cmd ~/scm-bin/lt $escArgv
    echo $cmd
    eval $cmd
end
