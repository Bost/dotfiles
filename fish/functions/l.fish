function l
    set escArgv (string escape -- $argv)
    if test -s ~/bin/exa
        # TODO consider custom coloring after `ls --color=never`
        set cmd exa --long --git --all --time-style=long-iso $escArgv
        # set cmd exa --long --git --all --extended --time=modified $escArgv
    else
        set cmd ls -lA --color "--time-style='+%d-%m-%Y %H:%M:%S'" $escArgv
    end
    echo $cmd
    eval $cmd
end
