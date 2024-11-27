function gcl --description "git clone & cd"
    # 'string escape' doesn't work for https://git.sr.ht/~krevedkokun/dotfiles
    # set escArgv (string escape -- $argv)

    set cmd ~/scm-bin/gcl $argv # gcl is implemented in Guile Scheme
    echo $cmd
    eval $cmd
    if test $status = 0
        # get the last parameter, remove .git suffix and call `basename` on it.
        set urlWithoutGit (string replace --regex "\.git\$" "" $argv[(count $argv)])
        set checkoutDir (basename $urlWithoutGit)

        set cmd cd $checkoutDir
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
