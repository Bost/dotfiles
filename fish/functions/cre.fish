function cre
    set pth $cheat/cmds
    set files $files $pth/emacs.el
    cheat-grep --grep-args="$argv" --files="$files"
end
