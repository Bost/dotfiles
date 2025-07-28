function lt --description "Listing w/ youngest on top"
    # set escArgv (string escape -- $argv)
    set escArgv $argv
    set cmd lt $escArgv
    echo $cmd
    eval $cmd
end
