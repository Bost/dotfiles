function crf
    set pth $dev/cheat/cmds
    set files $files $pth/findgrep.sh
    cheat-grep --grep-args="$argv" --files="$files"
end
