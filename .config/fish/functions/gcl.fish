function gcl
    # 'string escape' doesn't work for https://git.sr.ht/~krevedkokun/dotfiles
    # set escArgv (string escape -- $argv)
    set escArgv $argv
    set cmd ~/scm-bin/gcl $escArgv # gcl is implemented in Guile Scheme
    echo $cmd
    eval $cmd
    if test $status = 0
        # get the last command line parameter and call `basename` on it.
        set cmd cd (basename $escArgv[(count $escArgv)])
        echo $cmd
        eval $cmd
    end
end

## Test:
# rm -rf /tmp/foo/ /tmp/bar/
# mkcd /tmp/foo
## the '.io' is cut off from the 'ambrevar.gitlab.io':
# gcl https://gitlab.com/ambrevar/ambrevar.gitlab.io
#
# mkcd /tmp/bar
## clone to the correct directory works; 'cd ambrevar.gitlab.io' fails:
# gcl /tmp/foo/ambrevar.gitlab.io

