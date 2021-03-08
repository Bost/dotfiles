function l --description="ls -lA ..."
    set escArgv (string escape -- $argv)
    # exa initial run is slow and in general it takes a moment on a weaker HW
    # ~/bin/exa is a link to ~/bin/exa-linux-x86_64
    if false # test -s ~/bin/exa
        # TODO consider custom coloring after `ls --color=never`
        set cmd exa --long --git --all --time-style=long-iso $escArgv
        # set cmd exa --long --git --all --extended --time=modified $escArgv
    else
        set cmd ls -lA --color "--time-style='+%d-%m-%Y %H:%M:%S'" $escArgv
    end
    echo $cmd
    eval $cmd
end
