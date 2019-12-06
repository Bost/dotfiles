function crg
    set pth $dev/cheat/cmds
    set files $files $pth/git.sh
    cheat-grep --grep-args="$argv" --files="$files"
end
