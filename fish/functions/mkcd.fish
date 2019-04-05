function mkcd
    set dname (string escape -- $argv)
    # this doesn't work because of the command composition with ';'
    # set cmd (mkdir -p $dname; and cd $dname)
    set cmd mkdir -p $dname
    echo $cmd
    eval $cmd
    if test -d $dname
        set cmd1 cd $dname
        echo $cmd1
        eval $cmd1
    end
end
