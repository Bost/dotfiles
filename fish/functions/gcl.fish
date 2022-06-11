function gcl
    # 'string escape' doesn't work for https://git.sr.ht/~krevedkokun/dotfiles
    # set escArgv (string escape -- $argv)
    set escArgv $argv
    set cmd git clone $escArgv
    echo $cmd
    eval $cmd
    if test $status = 0
        set cmd cd (filename (basename $escArgv[(count $escArgv)]))
        echo $cmd
        eval $cmd
    end
end
