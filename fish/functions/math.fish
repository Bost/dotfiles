function math
    set escArgv (string escape -- $argv)
    echo "escArgv" $escArgv
    echo "argv" $argv
    # set cmd clj --eval $escArgv
    set cmd builtin math $escArgv
    echo $cmd
    eval $cmd
end
