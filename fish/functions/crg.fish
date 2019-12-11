function crg
    set pth $cheat/cmds
    set files $files $pth/git.sh
    cheat-grep --grep-args="$argv" --files="$files"
end
