function gcl
    # 'string escape' doesn't work for https://git.sr.ht/~krevedkokun/dotfiles
    # set escArgv (string escape -- $argv)
    set escArgv $argv
    set cmd ~/scm-bin/gcl $escArgv # gcl is implemented in Guile Scheme
    echo $cmd
    eval $cmd
    if test $status = 0
        if test -d $escArgv
            set dir $escArgv
        else
            set dir (filename (basename $escArgv[(count $escArgv)]))
        end
        set cmd cd $dir
        echo $cmd
        eval $cmd
    end
end
