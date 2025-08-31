# In bash a script is executes in a subshell, so the cd command only changes the
# directory within that subshell. So `gicl` for bash it it implemented as a
# function in .bashrc. See home-base.scm
function gicl --description "git-clone -- … & cd <checkoutDir>"
    git-clone -- $argv
    or return

    # Last argument (repo URL)
    set url $argv[-1]

    # Remove a possible trailing slash
    set url (string replace --regex '/$' '' -- $url)

    # Take the last path component
    set repo (basename $url)

    # Remove trailing .git suffix
    set repo (string replace --regex '\.git$' '' -- $repo)

    cd $repo
    or return
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
