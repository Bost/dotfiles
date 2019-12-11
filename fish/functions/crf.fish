function crf
    set pth $cheat/cmds
    set files $files $pth/findgrep.sh
    cheat-grep --grep-args="$argv" --files="$files"
end
