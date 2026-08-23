# -*- mode: fish -*-

## fish -n gicl.fish
## fish_indent --check gicl.fish

# A subprocess can't change its parent shell's working directory, so
# ~/scm-bin/gicl only clones and prints the resulting directory on stdout;
# see guix/home/common/scm-bin/git-clone.scm. The `cd` happens here, in the
# calling shell -- same idea as the `gicl` function in .bashrc.
function gicl --description "Clone a repo and cd into it (git clone)"
    set --local dir (command gicl -- $argv)
    or return

    cd $dir
    or return
end

## Test:
# rm -rf /tmp/foo/ /tmp/bar/
# mkcd /tmp/foo
## the '.io' is cut off from the 'ambrevar.gitlab.io':
# gicl https://gitlab.com/ambrevar/ambrevar.gitlab.io
#
# mkcd /tmp/bar
# gicl /tmp/foo/ambrevar.gitlab.io
#
## explicit target directory now also works:
# gicl https://gitlab.com/ambrevar/ambrevar.gitlab.io my-clone
