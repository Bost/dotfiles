function cre
    set pth $dev/cheat/cmds
    set files $files $pth/emacs.el
    cheat-grep --grep-args="$argv" --files="$files"
end
