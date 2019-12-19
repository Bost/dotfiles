function gcl
    set escArgv (string escape -- $argv)
    set cmd git clone $escArgv
    echo $cmd
    eval $cmd
    if test $status = 0
        set cmd cd (filename (basename $escArgv[(count $escArgv)]))
        echo $cmd
        eval $cmd
    end
end
