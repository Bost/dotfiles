function lT --description "Listing w/ oldest on top"
    # set escArgv (string escape -- $argv)
    set escArgv $argv
    set cmd lT $escArgv
    echo $cmd
    eval $cmd
end
