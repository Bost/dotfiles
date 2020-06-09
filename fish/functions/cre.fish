function cre
    set pth $cheat/cmds
    set files $files $pth/emacs.org
    cheat-grep --grep-args="$argv" --files="$files"
end
