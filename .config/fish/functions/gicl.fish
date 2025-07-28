# In bash a script is executes in a subshell, so the cd command only changes the
# directory within that subshell. So `gicl` for bash it it implemented as a
# function in .bashrc. See home-base.scm
function gicl --description "git clone … & cd <checkoutDir>"
    # 'string escape' doesn't work for https://git.sr.ht/~krevedkokun/dotfiles
    # set escArgv (string escape -- $argv)

    git-clone $argv # git-clone is implemented in Guile Scheme
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
